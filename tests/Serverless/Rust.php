<?php

namespace Tests\Serverless;

use Tests\Client;
use Tests\Serverless;

class Rust extends Serverless
{
    public function testLogs(): void
    {
        $response = Client::execute(headers: ['x-action' => 'logs']);
        $logId = $response['headers']['x-open-runtimes-log-id'];
        $logs = Client::getLogs($logId);
        $errors = Client::getErrors($logId);
        self::assertEquals(20, \strlen($logId));
        self::assertEquals(200, $response['code']);
        self::assertEmpty($response['body']);
        self::assertStringContainsString('Debug log', $logs);
        self::assertStringContainsString(42, $logs);
        self::assertStringContainsString(4.2, $logs);
        self::assertStringContainsString('true', \strtolower($logs)); // strlower allows True in Python
        self::assertStringContainsString('Native log', $logs);
        self::assertStringContainsString('Error log', $errors);
        self::assertStringContainsString('objectKey', $logs);
        self::assertStringContainsString('objectValue', $logs);
        self::assertStringContainsString('arrayValue', $logs);
        self::assertStringContainsString('Log+With+Plus+Symbol', $logs);
        self::assertStringContainsString("\n", $logs);
        self::assertStringContainsString('... Log truncated due to size limit (8000 characters)', $logs);
        self::assertStringContainsString('... Log truncated due to size limit (8000 characters)', $errors);
        self::assertGreaterThanOrEqual(9, \count(\explode("\n", $logs))); // Ensures each logs is on new line
        self::assertGreaterThanOrEqual(1, \count(\explode("\n", $errors))); // Ensures each error is on new line

        $response = Client::execute(headers: ['x-action' => 'logs']);
        $logIdSecond = $response['headers']['x-open-runtimes-log-id'];
        self::assertEquals(20, \strlen($logId));
        self::assertNotEquals($logId, $logIdSecond);

        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-logging' => 'disabled', 'x-open-runtimes-log-id' => 'noLogs']);
        $logs = Client::getLogs('noLogs');
        $errors = Client::getErrors('noLogs');
        self::assertEmpty($response['headers']['x-open-runtimes-log-id']);
        self::assertEmpty($logs);
        self::assertEmpty($errors);

        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-logging' => 'enabled']);
        $logs = Client::getLogs($response['headers']['x-open-runtimes-log-id']);
        $errors = Client::getErrors($response['headers']['x-open-runtimes-log-id']);
        self::assertNotEmpty($response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('Debug log', $logs);
        self::assertStringContainsString('Error log', $errors);

        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-log-id' => 'customLogs']);
        $logs = Client::getLogs('customLogs');
        $errors = Client::getErrors('customLogs');
        self::assertEquals('customLogs', $response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('Debug log', $logs);
        self::assertStringContainsString('Error log', $errors);
    }
}
