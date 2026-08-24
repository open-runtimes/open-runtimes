// Test driver for a single runtime. Replaces ci_tests.sh / tests.sh /
// formatter.sh / ci-helpers.sh / ci-cleanup.sh.
//
//   bun ci/test.ts node-25                 build image, stage fixtures, run tests
//   bun ci/test.ts node                    latest version of a runtime
//   bun ci/test.ts next-js                 SSR alias (enforced runtime)
//   bun ci/test.ts node-25 --skip-image    reuse existing open-runtimes/test-runtime
//   bun ci/test.ts node-25 --format-write  run formatters in write mode and exit
//
// Flow: bake image -> formatter check (latest only) -> stage fixtures ->
// tools -> build (+ variants by profile) -> serve trio -> startup metrics ->
// phpunit -> down.

import { existsSync, cpSync, rmSync, mkdirSync, writeFileSync, readdirSync } from 'fs';
import { join, dirname } from 'path';
import { resolveEntry, type Entry } from './common';

const repoRoot = join(dirname(Bun.main), '..');
const args = process.argv.slice(2);
const id = args.find((arg) => !arg.startsWith('--'));
if (!id) {
    console.error('Usage: bun ci/test.ts <runtime-id> [--skip-image] [--image-only] [--skip-formatter] [--format-write]');
    process.exit(1);
}

const entry = resolveEntry(id);
const composeEnv: Record<string, string> = {
    ...process.env as Record<string, string>,
    RUNTIME: entry.RUNTIME,
    VERSION: entry.VERSION,
    ENTRYPOINT: entry.ENTRYPOINT ?? '',
    ENTRYPOINT_NO_EXPORT: entry.ENTRYPOINT_NO_EXPORT ?? '',
    OUTPUT_DIRECTORY: entry.OUTPUT_DIRECTORY ?? '',
    INSTALL_COMMAND: entry.INSTALL_COMMAND ?? '',
    START_COMMAND: entry.START_COMMAND ?? '',
    TOOLS: entry.TOOLS ?? 'true',
    TEST_CLASS: entry.TEST_CLASS,
    COMPOSE_PROFILES: entry.COMPOSE_PROFILES.join(','),
};

const compose = ['docker', 'compose', '-f', join(repoRoot, 'tests/compose.yaml')];

function run(cmd: string[], env: Record<string, string> = composeEnv): void {
    console.log(`+ ${cmd.join(' ')}`);
    const result = Bun.spawnSync(cmd, { cwd: repoRoot, env, stdout: 'inherit', stderr: 'inherit' });
    if (result.exitCode !== 0) {
        console.error(`Command failed (exit ${result.exitCode}): ${cmd.join(' ')}`);
        process.exit(result.exitCode ?? 1);
    }
}

function buildImage(): void {
    run([
        'docker', 'buildx', 'bake', entry.BAKE_TARGET, '--load',
        '--set', `*.platform=${process.env.TEST_PLATFORM ?? 'linux/x86_64'}`,
        '--set', '*.tags=open-runtimes/test-runtime',
    ]);
}

function runFormatter(mode: 'check' | 'write'): void {
    const command = mode === 'check' ? entry.FORMATTER_CHECK : entry.FORMATTER_WRITE;
    const dirs = [join(repoRoot, 'runtimes', entry.RUNTIME_FOLDER)];
    const testResources = join(repoRoot, 'tests/resources/functions', entry.RUNTIME_FOLDER);
    if (existsSync(testResources)) {
        dirs.push(testResources);
    }
    for (const dir of dirs) {
        run([...compose, 'run', '--rm', 'formatter'], {
            ...composeEnv,
            FORMAT_DIR: dir,
            FORMATTER_PREPARE: entry.FORMATTER_PREPARE ?? 'true',
            FORMATTER_CMD: command ?? 'true',
        });
    }
}

function stageFixtures(): void {
    const testsDir = join(repoRoot, 'tests');
    const runtimeDir = join(testsDir, '.runtime');
    rmSync(runtimeDir, { recursive: true, force: true });
    mkdirSync(runtimeDir, { recursive: true });

    const functions = join(testsDir, 'resources/functions');
    if (entry.TEST_RESOURCE_DIR) {
        cpSync(join(functions, entry.TEST_RESOURCE_DIR), runtimeDir, { recursive: true });
    } else if (entry.ENFORCED_RUNTIME) {
        cpSync(join(functions, entry.RUNTIME), runtimeDir, { recursive: true });
    } else {
        cpSync(join(functions, entry.RUNTIME_FOLDER, 'latest'), runtimeDir, { recursive: true });
        const versionDir = join(functions, entry.RUNTIME_FOLDER, entry.VERSION_FOLDER);
        if (existsSync(versionDir)) {
            cpSync(versionDir, runtimeDir, { recursive: true });
        }
    }

    // Seed telemetry timings (download metrics are produced by the executor,
    // not the runtime; tests assert their presence)
    const telemetry = join(testsDir, 'resources/telemetry');
    mkdirSync(telemetry, { recursive: true });
    writeFileSync(join(telemetry, 'timings.txt'), 'local_download=0.200\nremote_download=10.560\n');

    // Prevent Docker from creating the archive path as a directory
    writeFileSync(join(runtimeDir, 'code.tar.gz'), '');

    rmSync('/tmp/logs', { recursive: true, force: true });
    mkdirSync('/tmp/logs', { recursive: true });
    rmSync('/tmp/startup-metrics.json', { force: true });
}

// Variant build directories: copies of tests/.runtime, staged AFTER the main
// build so the main build's /mnt/code does not contain them (java/kotlin/dotnet
// compile everything under the build directory — a nested copy of the sources
// produces duplicate-class errors). Matches the legacy ci_tests.sh ordering.
const variantDirs = ['no-export-build', 'modclean-disabled-build', 'nft-build'];

function stageVariant(variant: string): void {
    const runtimeDir = join(repoRoot, 'tests/.runtime');
    const dir = join(runtimeDir, variant, 'src');
    mkdirSync(dir, { recursive: true });
    for (const item of readdirSync(runtimeDir)) {
        if (variantDirs.includes(item)) {
            continue;
        }
        cpSync(join(runtimeDir, item), join(dir, item), { recursive: true });
    }
}

// Measure the primary serve container's startup: wall time from the container
// start until /__opr/health responds, plus the extract/prepare/startup
// breakdown the runtime reports on /__opr/timings. Results land in
// /tmp/startup-metrics.json for the CI comment step; failures are left to
// phpunit, which waits longer and reports container logs.
async function measureStartup(): Promise<void> {
    const port = process.env.HOST_PORT_MAIN ?? '3000';
    const base = `http://127.0.0.1:${port}`;
    const deadline = Date.now() + 120_000;

    // The compose file sets OPEN_RUNTIMES_SECRET=test-secret-key on the main
    // serve container; the static runtime gates every route (health included)
    // behind basic auth, so authenticate and count any HTTP response as ready.
    const secret = 'test-secret-key';
    const headers = {
        'x-open-runtimes-secret': secret,
        'authorization': `Basic ${Buffer.from(`opr:${secret}`).toString('base64')}`,
    };

    let ready = 0;
    while (ready === 0) {
        try {
            await fetch(`${base}/__opr/health`, { headers, signal: AbortSignal.timeout(1000) });
            ready = Date.now();
            break;
        } catch {
            // Server not accepting connections yet
        }
        if (Date.now() > deadline) {
            console.warn('Startup measurement skipped: server not ready within 120s.');
            return;
        }
        await Bun.sleep(50);
    }

    const inspect = Bun.spawnSync(['docker', 'inspect', '--format', '{{.State.StartedAt}}', 'open-runtimes-test-serve'], { env: composeEnv });
    const startedAt = Date.parse(inspect.stdout.toString().trim());
    if (Number.isNaN(startedAt)) {
        console.warn('Startup measurement skipped: could not inspect container start time.');
        return;
    }

    const metrics: Record<string, number> = {
        cold_start: Number(((ready - startedAt) / 1000).toFixed(3)),
    };

    try {
        const timings = await (await fetch(`${base}/__opr/timings`, { headers, signal: AbortSignal.timeout(5000) })).text();
        for (const line of timings.trim().split('\n')) {
            const [key, value] = line.split('=', 2);
            if (['extract', 'prepare', 'startup'].includes(key) && !Number.isNaN(parseFloat(value))) {
                metrics[key] = parseFloat(value);
            }
        }
    } catch {
        // Breakdown is best-effort; cold start alone is still worth reporting
    }

    const summary = Object.entries(metrics).map(([key, value]) => `${key}=${value.toFixed(3)}s`).join(' ');
    console.log(`Startup metrics: ${summary}`);
    writeFileSync('/tmp/startup-metrics.json', JSON.stringify(metrics));
}

function down(): void {
    Bun.spawnSync([...compose, 'down', '--remove-orphans'], { cwd: repoRoot, env: composeEnv });
    // Also remove any open-runtimes-* containers not created by compose
    // (e.g. leftovers from the legacy ci_tests.sh or manual runs)
    const list = Bun.spawnSync(['docker', 'ps', '-aq', '-f', 'name=open-runtimes-'], { env: composeEnv });
    const ids = list.stdout.toString().trim().split('\n').filter(Boolean);
    if (ids.length > 0) {
        Bun.spawnSync(['docker', 'rm', '--force', ...ids], { env: composeEnv });
    }
}

// ─── Flow ────────────────────────────────────────────────────────────────────

if (!args.includes('--skip-image')) {
    buildImage();
}

if (args.includes('--image-only')) {
    process.exit(0);
}

// The compose services join the external `openruntimes` network — make sure it
// exists before the first compose invocation (the formatter runs early)
down();
Bun.spawnSync(['docker', 'network', 'create', 'openruntimes'], { env: composeEnv });

if (args.includes('--format-write')) {
    runFormatter('write');
    process.exit(0);
}

if (entry.RUN_FORMATTER && !args.includes('--skip-formatter')) {
    console.log('Running formatter ...');
    runFormatter('check');
} else if (!entry.RUN_FORMATTER) {
    console.log(`Skipping formatter. Formatter runs only for the latest version, and not for enforced-runtime aliases.`);
}

console.log('Staging test fixtures ...');
stageFixtures();

console.log('Testing tools ...');
run([...compose, 'run', '--rm', 'tools']);

console.log('Running build ...');
run([...compose, 'run', '--rm', 'build']);
if (entry.COMPOSE_PROFILES.includes('no-export')) {
    console.log('Building no-export entrypoint test ...');
    stageVariant('no-export-build');
    run([...compose, 'run', '--rm', 'build-no-export']);
}
if (entry.COMPOSE_PROFILES.includes('cleanup-variants')) {
    console.log('Building cleanup-disabled baseline ...');
    stageVariant('modclean-disabled-build');
    run([...compose, 'run', '--rm', 'build-baseline']);
    console.log('Building NFT-enabled test ...');
    stageVariant('nft-build');
    run([...compose, 'run', '--rm', 'build-nft']);
}

console.log('Starting runtime servers ...');
run([...compose, 'up', '-d']);

console.log('Measuring startup time ...');
await measureStartup();

console.log('Running tests ...');
run([...compose, 'run', '--rm', 'phpunit']);

down();
console.log('Done.');
