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
            $response = Client::execute(url: '/concurrency', method: 'GET');
            self::assertEquals(200, $response['code']);
            self::assertStringContainsString("OK Response", $response['body']);
            $logs = Client::getLogs($response['headers']['x-open-runtimes-log-id']);
            self::assertStringContainsString('Concurrent Log 1', $logs);
            self::assertStringContainsString('Concurrent Log 2', $logs);
            self::assertStringContainsString('Concurrent Log 3', $logs);
            self::assertEmpty(Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    
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
        self::assertNotEquals("12345", \implode("", $finishes));
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
        self::assertStringNotContainsString('Code exception occured', Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('Code exception occured', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
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
}
