<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class BaseDev extends TestCase
{
    public function setUp(): void
    {
        Client::$port = 3001;
    }

    public function tearDown(): void
    {
    }

    // Keep always first, it tests disabled logging. Must be done first
    public function testDevLogFiles(): void
    {
        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-logging' => 'disabled' ]);
        $logs = Client::getLogs('dev');
        $errors = Client::getErrors('dev');
        self::assertEmpty($response['headers']['x-open-runtimes-log-id']);
        self::assertEmpty($logs);
        self::assertEmpty($errors);

        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-logging' => 'enabled' ]);
        $logs = Client::getLogs('dev');
        $errors = Client::getErrors('dev');
        self::assertEquals('dev', $response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('Debug log', $logs);
        self::assertStringContainsString('Error log', $errors);

        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-logging' => 'enabled', 'x-open-runtimes-log-id' => 'myLog' ]);
        $logs = Client::getLogs('myLog');
        $errors = Client::getErrors('myLog');
        self::assertEquals('myLog', $response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('Debug log', $logs);
        self::assertStringContainsString('Error log', $errors);
    }

    public function testEmptySecret(): void
    {
        $response = Client::execute(headers: ['x-action' => 'plaintextResponse']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('Hello World 👋', $response['body']);

        $response = Client::execute(headers: ['x-action' => 'plaintextResponse', 'x-open-runtimes-secret' => 'wrong-secret']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('Hello World 👋', $response['body']);
    }
}