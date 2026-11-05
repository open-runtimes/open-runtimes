<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class XStatic extends Base
{
    public function testMountedCodePath(): void
    {
        $image = trim(shell_exec("docker inspect --format='{{.Image}}' open-runtimes-test-serve") ?? '');
        self::assertNotEmpty($image);
        $script = <<<'BASH'
set -e
mkdir -p "$OPEN_RUNTIMES_CODE_PATH" /mnt/telemetry
printf 'Mounted website' >"$OPEN_RUNTIMES_CODE_PATH/index.html"
printf 'Mounted asset' >"$OPEN_RUNTIMES_CODE_PATH/asset.txt"
: >"$OPEN_RUNTIMES_CODE_PATH/.open-runtimes"
printf 'local_download=0.200\n' >/mnt/telemetry/timings.txt
exec bash helpers/start.sh "bash helpers/server.sh"
BASH;
        $container = 'open-runtimes-test-static-code-path';
        try {
            exec('docker run -d --name ' . escapeshellarg($container)
                . ' --network openruntimes --entrypoint bash'
                . ' -e OPEN_RUNTIMES_CODE_PATH=' . escapeshellarg('/mnt/workspace/code with spaces')
                . ' -e OPEN_RUNTIMES_STATIC_FALLBACK=index.html'
                . ' -e OPEN_RUNTIMES_SECRET=test-secret-key '
                . escapeshellarg($image) . ' -c ' . escapeshellarg($script), $output, $code);
            self::assertSame(0, $code, implode("\n", $output));
            Client::$host = $container;
            $this->awaitHostReady();

            foreach (['/index.html' => 'Mounted website', '/asset.txt' => 'Mounted asset', '/missing' => 'Mounted website'] as $path => $body) {
                $response = Client::execute(url: $path, method: 'GET');
                self::assertEquals(200, $response['code']);
                self::assertEquals($body, $response['body']);
            }
            $this->testHealth();
            $response = Client::execute(url: '/__opr/timings', method: 'GET');
            self::assertEquals(200, $response['code']);
            self::assertStringContainsString('local_download=0.200', $response['body']);
        } finally {
            shell_exec('docker rm -f ' . escapeshellarg($container) . ' 2>&1');
            Client::$host = 'open-runtimes-test-serve';
        }
    }

    public function testCacheHeader(): void
    {
        
        $response = Client::execute(url: '/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertArrayHasKey('cdn-cache-control', $response['headers']);
        self::assertArrayNotHasKey('surrogate-control', $response['headers']);
        self::assertEquals('public, max-age=36000', $response['headers']['cdn-cache-control']);
        
        $response = Client::execute(url: '/some/nested/path.txt', method: 'GET');
        self::assertArrayHasKey('cdn-cache-control', $response['headers']);
        self::assertArrayNotHasKey('surrogate-control', $response['headers']);
        
        Client::$host = 'open-runtimes-test-serve-secondary';
        
        $response = Client::execute(url: '/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertArrayNotHasKey('cdn-cache-control', $response['headers']);
        self::assertArrayHasKey('surrogate-control', $response['headers']);
        self::assertEquals('public, max-age=36000', $response['headers']['surrogate-control']);
        
        $response = Client::execute(url: '/some/nested/path.txt', method: 'GET');
        self::assertArrayNotHasKey('cdn-cache-control', $response['headers']);
        self::assertArrayHasKey('surrogate-control', $response['headers']);
        
        Client::$host = 'open-runtimes-test-serve';
    }
    
    public function testFiles(): void
    {
        $response = Client::execute(url: '/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website title', $response['body']);
        self::assertStringContainsString('Website body content', $response['body']);
        self::assertStringContainsString('src="main.js"', $response['body']);

        $response = Client::execute(url: '/main.js', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('JavaScript log', $response['body']);
    }

    public function testCleanUrls(): void
    {
        $response = Client::execute(url: '/contact.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Contact page content', $response['body']);

        $response = Client::execute(url: '/contact', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Contact page content', $response['body']);

        $response = Client::execute(url: '/about-us/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('About us page contents', $response['body']);

        $response = Client::execute(url: '/about-us/', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('About us page contents', $response['body']);
    }

    public function testFallbackPage() {
        $response = Client::execute(url: '/doesntexist', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        $response = Client::execute(url: '/doesntexist/', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        $response = Client::execute(url: '/doesntexist.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);
    }

    public function testTrailingSlashRedirect()
    {
        $response = Client::execute(url: '/about-us', method: 'GET');
        self::assertEquals(308, $response['code']);
        self::assertEquals("/about-us/", $response['headers']['location']);
    }

    public function testFallbackFile()
    {
        $response = Client::execute(url: '/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        $response = Client::execute(url: '/missing.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        Client::$host = 'open-runtimes-test-serve-secondary';

        $response = Client::execute(url: '/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        $response = Client::execute(url: '/missing.html', method: 'GET');
        self::assertEquals(404, $response['code']);
        self::assertStringContainsString('404', $response['body']);
        self::assertStringContainsString('Page not found', $response['body']);
        self::assertStringNotContainsString('Website body content', $response['body']);

        Client::$host = 'open-runtimes-test-serve-teritary';

        $response = Client::execute(url: '/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        $response = Client::execute(url: '/missing.html', method: 'GET');
        self::assertEquals(404, $response['code']);
        self::assertStringContainsString('Custom-branded 404 page', $response['body']);
        self::assertStringNotContainsString('Website body content', $response['body']);

        Client::$host = 'open-runtimes-test-serve';
    }

    // TODO: Improve in future, to also ensure build step sees hidden files
    public function testHiddenFile(): void
    {
         Client::$host = 'open-runtimes-test-serve-secondary';
         
        $response = Client::execute(url: '/.config/.file', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('VISIBLE=yes', $response['body']);

        $response = Client::execute(url: '/.hidden', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('VISIBLE=yes', $response['body']);

        $response = Client::execute(url: '/.gitignore', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('!.env', $response['body']);

        $response = Client::execute(url: '/.well-known/apple-app-site-association', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('ABCDE12345.com.example.myapp', $response['body']);

        $response = Client::execute(url: '/.well-known/assetlinks.json', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('com.example.myapp', $response['body']);

        $response = Client::execute(url: '/.env', method: 'GET');
        self::assertEquals(404, $response['code']);
        self::assertStringContainsString('Page not found', $response['body']);

        $response = Client::execute(url: '/.env.dev', method: 'GET');
        self::assertEquals(404, $response['code']);
        self::assertStringContainsString('Page not found', $response['body']);

        $response = Client::execute(url: '/.env.anysuffix', method: 'GET');
        self::assertEquals(404, $response['code']);
        self::assertStringContainsString('Page not found', $response['body']);

        $response = Client::execute(url: '/.config/.env', method: 'GET');
        self::assertEquals(404, $response['code']);
        self::assertStringContainsString('Page not found', $response['body']);
        
        Client::$host = 'open-runtimes-test-serve';
    }
}
