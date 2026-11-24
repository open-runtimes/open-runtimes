<?php

namespace Tests;

use Swoole\Coroutine as Co;

class SSR extends CSR
{
    public function testHomepagePrerendered(): void
    {
        $response = Client::execute(url: '/', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString("Hello Open Runtimes", $response['body']);
    }

    public function testServerAction(): void
    {
        $scrapeDate = function (string $body) {
            $date = \explode('[DATE_START]', $body)[1];
            $date = \explode('[DATE_END]', $body)[0];
            return $date;
        };

        $response = Client::execute(url: '/date', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertNotEmpty($response['body']);
        $body1 = $response['body'];
        $date1 = $scrapeDate($body1);

        \sleep(1);

        $response = Client::execute(url: '/date', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertNotEmpty($response['body']);
        $body2 = $response['body'];
        $date2 = $scrapeDate($body2);

        self::assertNotEquals($body1, $body2);
        self::assertNotEquals($date1, $date2);
    }

    public function testServerLogs(): void
    {
        $response = Client::execute(url: '/logs', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString("All logs printed", $response['body']);
        self::assertStringContainsString('A log printed', Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('An error printed', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
        $logId1 = $response['headers']['x-open-runtimes-log-id'];

        $response = Client::execute(url: '/logs', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString("All logs printed", $response['body']);
        self::assertStringContainsString('A log printed', Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('An error printed', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
        $logId2 = $response['headers']['x-open-runtimes-log-id'];

        self::assertEquals(20, \strlen($logId1));
        self::assertEquals(20, \strlen($logId2));
        self::assertNotEquals($logId1, $logId2);

        $response = Client::execute(url: '/logs', method: 'GET', headers: ['x-open-runtimes-logging' => 'disabled', 'x-open-runtimes-log-id' => 'noLogs' ]);
        self::assertEmpty($response['headers']['x-open-runtimes-log-id']);
        self::assertEmpty(Client::getLogs('noLogs'));
        self::assertEmpty(Client::getErrors('noLogs'));

        $response = Client::execute(url: '/logs', method: 'GET', headers: ['x-open-runtimes-logging' => 'enabled' ]);
        self::assertNotEmpty($response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('A log printed', Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('An error printed', Client::getErrors($response['headers']['x-open-runtimes-log-id']));

        $response = Client::execute(url: '/logs', method: 'GET', headers: ['x-open-runtimes-log-id' => 'customLogs' ]);
        self::assertEquals('customLogs', $response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('A log printed', Client::getLogs('customLogs'));
        self::assertStringContainsString('An error printed', Client::getErrors('customLogs'));
    }


    public function testServerLogsConcurrency(): void
    {
        $ensureRequestLogs = function() {
            $start = \microtime(true);
            $response = Client::execute(url: '/concurrency', method: 'GET');
            $end = \microtime(true);

            self::assertEquals(200, $response['code']);
            self::assertStringContainsString("OK Response", $response['body']);
            $logs = Client::getLogs($response['headers']['x-open-runtimes-log-id']);
            self::assertStringContainsString('Concurrent Log 1', $logs);
            self::assertStringContainsString('Concurrent Log 2', $logs);
            self::assertStringContainsString('Concurrent Log 3', $logs);
            self::assertEmpty(Client::getErrors($response['headers']['x-open-runtimes-log-id']));
            self::assertGreaterThan(1.5, $end - $start); // 3 * 500ms (minimum for each log)
    
            $log1Pos = \strpos($logs, 'Concurrent Log 1');
            $log2Pos = \strpos($logs, 'Concurrent Log 2');
            $log3Pos = \strpos($logs, 'Concurrent Log 3');
    
            self::assertIsNumeric($log1Pos);
            self::assertIsNumeric($log2Pos);
            self::assertIsNumeric($log3Pos);
    
            self::assertGreaterThan($log2Pos, $log3Pos); // log 3 larger than log 2
            self::assertGreaterThan($log1Pos, $log2Pos); // log 2 larger than log 1
        };

        $finishes = [];

        $ensureRequestLogs();

        Co\run(function () use ($ensureRequestLogs, &$finishes) {
            Co::join([
                Co\go(function () use ($ensureRequestLogs, &$finishes) {
                    $ensureRequestLogs();
                    \array_push($finishes, 1);
                }),
                Co\go(function () use ($ensureRequestLogs, &$finishes) {
                    $ensureRequestLogs();
                    \array_push($finishes, 2);
                }),
                Co\go(function () use ($ensureRequestLogs, &$finishes) {
                    $ensureRequestLogs();
                    \array_push($finishes, 3);
                }),
                Co\go(function () use ($ensureRequestLogs, &$finishes) {
                    $ensureRequestLogs();
                    \array_push($finishes, 4);
                }),
                Co\go(function () use ($ensureRequestLogs, &$finishes) {
                    $ensureRequestLogs();
                    \array_push($finishes, 5);
                }),
            ]);
        });

        self::assertCount(5, $finishes);
        self::assertNotEquals("12345", \implode("", $finishes)); // 0.8% flakyness. Re-run when it happens
    }

    public function testDevLogFiles(): void
    {
        Client::$host = 'open-runtimes-test-serve-secondary';

        // Cleanup
        $response = \shell_exec('rm -rf /tmp/logs/dev_logs.log && echo $?');
        self::assertStringEndsWith("0\n", $response); // Exit code 0 means success
        $response = \shell_exec('rm -rf /tmp/logs/dev_errors.log && echo $?');
        self::assertStringEndsWith("0\n", $response); // Exit code 0 means success

        $response = Client::execute(url: '/logs', method: 'GET', headers: ['x-open-runtimes-logging' => 'disabled' ]);
        self::assertEmpty($response['headers']['x-open-runtimes-log-id']);
        self::assertEmpty(Client::getLogs('dev'));
        self::assertEmpty(Client::getErrors('dev'));
 
        $response = Client::execute(url: '/logs', method: 'GET', headers: ['x-open-runtimes-logging' => 'enabled' ]);
        self::assertEquals('dev', $response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('A log printed', Client::getLogs('dev'));
        self::assertStringContainsString('An error printed', Client::getErrors('dev'));
 
        $response = Client::execute(url: '/logs', method: 'GET', headers: ['x-open-runtimes-logging' => 'enabled', 'x-open-runtimes-log-id' => 'myLog' ]);
        self::assertEquals('myLog', $response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('A log printed', Client::getLogs('myLog'));
        self::assertStringContainsString('An error printed', Client::getErrors('myLog'));

        Client::$host = 'open-runtimes-test-serve';
    }

    public function testServerException(): void
    {
        $response = Client::execute(url: '/exception', method: 'GET');
        self::assertEquals(500, $response['code']);
        self::assertStringNotContainsString("No exceptions", $response['body']);
        self::assertStringNotContainsString('Code exception occurred', Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('Code exception occurred', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }

    public function testServerLibrary(): void
    {
        $scrapeUuid = function (string $body) {
            $date = \explode('[UUID_START]', $body)[1];
            $date = \explode('[UUID_END]', $body)[0];
            return $date;
        };

        $response = Client::execute(url: '/library', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString("My UUID is", $response['body']);
        $uuid1 = $scrapeUuid($response['body']);

        $response = Client::execute(url: '/library', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString("My UUID is", $response['body']);
        $uuid2 = $scrapeUuid($response['body']);

        self::assertNotEquals($uuid1, $uuid2);
    }

    public function testHiddenFile(): void
    {
        $response = Client::execute(url: '/hidden', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('HIDDEN_FILE', $response['body']);
    }
    
    public function testModclean(): void
    {
        // Main build has modclean enabled (default behavior)
        $entries = $this->listBuildArchiveEntries('/app/tests/.runtime');
        self::assertSame(0, \preg_match('/node_modules\/.*\.md$/m', $entries), 'Expected no .md files in node_modules when modclean is enabled');

        // Baseline build has all cleanup disabled
        $entries = $this->listBuildArchiveEntries('/app/tests/.runtime/modclean-disabled-build/src');
        self::assertSame(1, \preg_match('/node_modules\/.*\.md$/m', $entries), 'Expected .md files in node_modules when modclean is disabled');
    }

    public function testNft(): void
    {
        // NFT build has NFT enabled, modclean disabled
        $nftCount = \substr_count($this->listBuildArchiveEntries('/app/tests/.runtime/nft-build/src'), 'node_modules/');

        // Baseline build has all cleanup disabled
        $fullCount = \substr_count($this->listBuildArchiveEntries('/app/tests/.runtime/modclean-disabled-build/src'), 'node_modules/');

        self::assertGreaterThan(0, $fullCount, 'Expected files in node_modules when cleanup is disabled');
        self::assertLessThan($fullCount, $nftCount, 'Expected fewer files in node_modules after NFT pruning');
    }

    public function testProductionDependencies(): void
    {
        $directory = '/app/tests/.runtime/production-build/src';
        $baseline = $this->listBuildArchiveEntries('/app/tests/.runtime/modclean-disabled-build/src');
        $entries = $this->listBuildArchiveEntries($directory);

        // A single committed npm lockfile qualifies, except for Next.js. Other
        // committed lockfiles fall back. Without one, the install command
        // writes its own lockfile during the build, so either outcome is valid.
        $lockfiles = \array_values(\array_filter(
            ['package-lock.json', 'npm-shrinkwrap.json', 'yarn.lock', 'pnpm-lock.yaml', 'bun.lock', 'bun.lockb'],
            fn (string $file) => \is_file($directory . '/' . $file)
        ));
        $nextjs = \str_contains($baseline, '.next/');
        $metadata = $this->readBuildArchiveFile($directory, '.open-runtimes');
        $production = \str_contains($metadata, 'OPEN_RUNTIMES_CLEANUP=production');
        if ($nextjs || ($lockfiles !== [] && $lockfiles !== ['package-lock.json'])) {
            self::assertFalse($production, 'Expected the fallback cleanup');
            return;
        }
        if ($lockfiles === ['package-lock.json']) {
            self::assertTrue($production, 'Expected the production install');
        } elseif (!$production) {
            return;
        }

        $count = \substr_count($entries, 'node_modules/');
        self::assertGreaterThan(0, $count, 'Expected production dependencies in node_modules');

        $manifest = \json_decode(\file_get_contents($directory . '/package.json'), true);
        foreach (\array_keys($manifest['dependencies'] ?? []) as $name) {
            self::assertStringContainsString('node_modules/' . $name . '/package.json', $entries, "Expected dependency {$name}");
        }

        if (empty($manifest['devDependencies'])) {
            return;
        }
        self::assertLessThan(\substr_count($baseline, 'node_modules/'), $count, 'Expected fewer files in node_modules without devDependencies');
        $dropped = \array_filter(
            \array_keys($manifest['devDependencies']),
            fn (string $name) => \str_contains($baseline, 'node_modules/' . $name . '/package.json')
                && !\str_contains($entries, 'node_modules/' . $name . '/package.json')
        );
        self::assertNotEmpty($dropped, 'Expected devDependencies to be left out');
    }

    protected function readBuildArchiveFile(string $directory, string $file): string
    {
        $squashfs = $directory . '/code.sqfs';
        if (\is_file($squashfs) && \filesize($squashfs) > 0) {
            return \shell_exec('unsquashfs -cat ' . \escapeshellarg($squashfs) . ' ' . \escapeshellarg($file) . ' 2>/dev/null') ?? '';
        }

        return \shell_exec('tar -xzOf ' . \escapeshellarg($directory . '/code.tar.gz') . ' ' . \escapeshellarg('./' . $file) . ' 2>/dev/null') ?? '';
    }

    protected function listBuildArchiveEntries(string $directory): string
    {
        $squashfs = $directory . '/code.sqfs';
        if (\is_file($squashfs) && \filesize($squashfs) > 0) {
            return \shell_exec('unsquashfs -l ' . \escapeshellarg($squashfs) . ' 2>/dev/null') ?? '';
        }

        $tar = $directory . '/code.tar.gz';
        if (\is_file($tar) && \filesize($tar) > 0) {
            return \shell_exec('tar -tzf ' . \escapeshellarg($tar) . ' 2>/dev/null') ?? '';
        }

        self::fail('Build archive not found in ' . $directory);
    }

    public function testStaticCache(): void
    {   
        $response = Client::execute(url: '/static.txt', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertArrayHasKey('cdn-cache-control', $response['headers']);
        self::assertArrayNotHasKey('surrogate-control', $response['headers']);
        self::assertEquals('public, max-age=36000', $response['headers']['cdn-cache-control']);
        
        $response = Client::execute(url: '/date', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertArrayNotHasKey('cdn-cache-control', $response['headers']);
        self::assertArrayNotHasKey('surrogate-control', $response['headers']);
        
        Client::$host = 'open-runtimes-test-serve-secondary';
        
        $response = Client::execute(url: '/static.txt', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertArrayNotHasKey('cdn-cache-control', $response['headers']);
        self::assertArrayHasKey('surrogate-control', $response['headers']);
        self::assertEquals('public, max-age=36000', $response['headers']['surrogate-control']);

        $response = Client::execute(url: '/date', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertArrayNotHasKey('cdn-cache-control', $response['headers']);
        self::assertArrayNotHasKey('surrogate-control', $response['headers']);
        
        Client::$host = 'open-runtimes-test-serve';
    }   
}
