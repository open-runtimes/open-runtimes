"""Container-level shutdown regressions. See docs/shutdown.md for invocation."""
import concurrent.futures
import http.client
import os
from pathlib import Path
import subprocess
import shutil
import tempfile
import time
import unittest

ROOT = Path(__file__).resolve().parents[2]
IMAGE = os.environ.get('SHUTDOWN_TEST_IMAGE', 'open-runtimes/test-runtime')


def docker(*args, timeout=30):
    return subprocess.check_output(['docker', *args], text=True, timeout=timeout).strip()


class ShutdownTest(unittest.TestCase):
    def setUp(self):
        self.directory = tempfile.TemporaryDirectory()
        self.path = Path(self.directory.name)
        (self.path / '.extracted').touch()
        (self.path / '.open-runtimes').touch()
        (self.path / 'server.cjs').write_text('''
const http = require('http');
const server = http.createServer((req, res) => {
  if (req.url === '/hang') { console.log('request started'); return; }
  if (req.url === '/slow') {
    console.log('request started');
    setTimeout(() => {
      res.end('completed');
      for (let i = 0; i < 1000; i++) console.log('final log ' + i);
      process.stdout.write('last partial log');
    }, 1500);
  } else { res.end('OK'); }
});
require('/usr/local/server/helpers/http-shutdown.cjs')(server);
server.listen(3000, '0.0.0.0', () => console.log('HTTP server successfully started!'));
''')
        self.container = None
        self.helpers = ROOT / "helpers"

    def tearDown(self):
        if self.container:
            subprocess.run(['docker', 'rm', '-f', self.container], capture_output=True)
        self.directory.cleanup()

    def start(self, command='exec node src/function/server.cjs'):
        self.container = docker(
            'create', '--entrypoint', 'bash', '-p', '127.0.0.1::3000',
            '-e', 'OPEN_RUNTIMES_SECRET=', '-e', 'OPEN_RUNTIMES_ENTRYPOINT=index.js',
            '-v', f'{self.helpers}:/usr/local/server/helpers:ro',
            '-v', f'{ROOT / "runtimes/node/versions/latest/src/server.js"}:/usr/local/server/src/server.js:ro',
            '-v', f'{ROOT / "runtimes/node/versions/latest/src/logger.js"}:/usr/local/server/src/logger.js:ro',
            '-v', f'{ROOT / "runtimes/node/versions/latest/src/config.js"}:/usr/local/server/src/config.js:ro',
            '--tmpfs', '/usr/local/server/src/function',
            '-v', f'{self.path}:/mnt/code:ro', IMAGE,
            'helpers/start.sh', command,
        )
        docker('start', self.container)

    def ready(self):
        self.wait_log('HTTP server successfully started!')
        binding = docker('port', self.container, '3000/tcp')
        self.port = int(binding.rsplit(':', 1)[1])

    def wait_log(self, message):
        deadline = time.monotonic() + 15
        while time.monotonic() < deadline:
            logs = docker('logs', self.container)
            if message in logs:
                return
            if docker('inspect', '-f', '{{.State.Running}}', self.container) != 'true':
                self.fail(f'Container exited before {message!r}: {logs}')
            time.sleep(0.05)
        self.fail(f'Missing {message!r}: {logs}')

    def request(self, path='/'):
        connection = http.client.HTTPConnection('127.0.0.1', self.port, timeout=10)
        try:
            connection.request('GET', path, headers={'x-open-runtimes-logging': 'disabled'})
            response = connection.getresponse()
            return response.status, response.read()
        finally:
            connection.close()

    def signal(self, signal='TERM'):
        docker('kill', '--signal', signal, self.container)

    def exit_code(self):
        return int(docker('wait', self.container, timeout=12))

    def test_drain_and_final_logs(self):
        self.start()
        self.ready()
        idle = http.client.HTTPConnection('127.0.0.1', self.port, timeout=5)
        idle.request('GET', '/')
        idle.getresponse().read()
        with concurrent.futures.ThreadPoolExecutor() as pool:
            response = pool.submit(self.request, '/slow')
            self.wait_log('request started')
            self.signal()
            self.signal()  # Repeated signals must not interrupt the drain.
            self.assertEqual(response.result(), (200, b'completed'))
        idle.close()
        self.assertEqual(self.exit_code(), 0)
        logs = docker('logs', self.container)
        self.assertIn('final log 999', logs)
        self.assertIn('last partial log', logs)
        with self.assertRaises(OSError):
            self.request()

    def test_sigint(self):
        self.start()
        self.ready()
        self.signal('INT')
        self.assertEqual(self.exit_code(), 0)

    def test_hung_request_is_bounded(self):
        self.start()
        self.ready()
        with concurrent.futures.ThreadPoolExecutor() as pool:
            response = pool.submit(self.request, '/hang')
            self.wait_log('request started')
            started = time.monotonic()
            docker('stop', '--time', '1', self.container)
            self.assertEqual(self.exit_code(), 137)
            self.assertLess(time.monotonic() - started, 5)
            with self.assertRaises((OSError, http.client.HTTPException)):
                response.result()

    def test_sigkill(self):
        self.start()
        self.ready()
        self.signal('KILL')
        self.assertEqual(self.exit_code(), 137)

    def test_failure_exit_status(self):
        self.start('exit 42')
        self.assertEqual(self.exit_code(), 42)

    def test_child_receives_signal(self):
        (self.path / 'child.cjs').write_text("""
process.on('SIGTERM', () => {
  setTimeout(() => { console.log('child drained'); process.exit(0); }, 300);
});
setInterval(() => {}, 1000);
console.log('child ready');
""")
        with (self.path / 'server.cjs').open('a') as server:
            server.write("\nrequire('child_process').spawn(process.execPath, ['src/function/child.cjs'], {stdio: 'inherit'});\n")
        self.start()
        self.ready()
        self.wait_log('child ready')
        self.signal()
        self.assertEqual(self.exit_code(), 0)
        self.assertIn('child drained', docker('logs', self.container))

    def test_stop_before_server_ready(self):
        self.start('echo startup-active; exec sleep 60')
        self.wait_log('startup-active')
        self.signal()
        self.assertEqual(self.exit_code(), 143)

    def test_signal_during_launch_window(self):
        self.helpers = self.path / 'helpers'
        shutil.copytree(ROOT / 'helpers', self.helpers)
        lifecycle = self.helpers / 'lifecycle/start.sh'
        lifecycle.write_text(lifecycle.read_text().replace(
            'bash -c "$1" 2>&1',
            'echo launch-window; sleep 60\nbash -c "$1" 2>&1',
        ))
        self.start()
        self.wait_log('launch-window')
        self.signal()
        self.assertEqual(self.exit_code(), 143)
        self.assertNotIn('HTTP server successfully started!', docker('logs', self.container))

    def test_matrix_drain_check(self):
        (self.path / 'index.js').write_text("""
module.exports = async ({log, res}) => {
  log('Timeout start.');
  await new Promise(resolve => setTimeout(resolve, 1500));
  log('Timeout end.');
  return res.text('Successful response.');
};
""")
        self.start('exec bash helpers/server.sh')
        self.ready()
        check = self.path / 'check.ts'
        check.write_text(
            f'import {{checkShutdown}} from "{ROOT / "ci/shutdown.ts"}";\n'
            'await checkShutdown(Bun.argv[2], Bun.argv[3], true);\n'
        )
        subprocess.run(['bun', str(check), self.container, f'http://127.0.0.1:{self.port}'], check=True, timeout=15)
        self.assertEqual(self.exit_code(), 0)

    def test_node_function_drain(self):
        (self.path / 'index.js').write_text('''
module.exports = async ({res}) => {
  require('fs').writeFileSync('/tmp/request-started', 'yes');
  await new Promise(resolve => setTimeout(resolve, 1500));
  return res.text('completed');
};
''')
        self.start('exec bash helpers/server.sh')
        self.ready()
        with concurrent.futures.ThreadPoolExecutor() as pool:
            response = pool.submit(self.request)
            deadline = time.monotonic() + 10
            while time.monotonic() < deadline:
                found = subprocess.run(['docker', 'exec', self.container, 'test', '-f', '/tmp/request-started'])
                if found.returncode == 0:
                    break
                time.sleep(0.05)
            else:
                self.fail('Function did not start')
            self.signal()
            self.assertEqual(response.result(), (200, b'completed'))
        self.assertEqual(self.exit_code(), 0)


if __name__ == '__main__':
    unittest.main()
