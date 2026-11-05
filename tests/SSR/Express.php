<?php

namespace Tests\SSR;

use Tests\Base;
use Tests\Client;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class Express extends Base
{
    public function testHomepage(): void
    {
        $response = Client::execute(url: '/', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertEquals('Hello Open Runtimes', $response['body']);
    }

    public function testRouting(): void
    {
        $response = Client::execute(url: '/users/42?sort=asc', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertEquals(['id' => '42', 'sort' => 'asc'], \json_decode($response['body'], true));

        $response = Client::execute(url: '/missing', method: 'GET');
        self::assertEquals(404, $response['code']);
    }

    public function testJsonBody(): void
    {
        $response = Client::execute(body: ['name' => 'Open Runtimes'], url: '/echo', method: 'POST', headers: ['content-type' => 'application/json']);
        self::assertEquals(201, $response['code']);
        self::assertEquals(['name' => 'Open Runtimes'], \json_decode($response['body'], true));
    }

    public function testEnvironment(): void
    {
        $response = Client::execute(url: '/env', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertEquals('customValue', $response['body']);
    }

    public function testLogs(): void
    {
        $response = Client::execute(url: '/logs', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertEquals('All logs printed', $response['body']);

        $logId = $response['headers']['x-open-runtimes-log-id'];
        self::assertStringContainsString('A log printed', Client::getLogs($logId));
        self::assertStringContainsString('An error printed', Client::getErrors($logId));
    }

    public function testException(): void
    {
        $response = Client::execute(url: '/exception', method: 'GET');
        self::assertEquals(500, $response['code']);
        self::assertStringContainsString('Code exception occurred', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }

    public function testHealth(): void
    {
        $response = Client::execute(url: '/__opr/health', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertEquals('OK', $response['body']);
    }

    public function testTimeout(): void
    {
        $response = Client::execute(url: '/slow', method: 'GET', headers: ['x-open-runtimes-timeout' => '1']);
        self::assertEquals(500, $response['code']);
        self::assertStringContainsString('Execution timed out.', Client::getErrors($response['headers']['x-open-runtimes-log-id']));

        \sleep(3);

        $response = Client::execute(url: '/slow', method: 'GET', headers: ['x-open-runtimes-timeout' => '5']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('Slow response', $response['body']);
    }
}
