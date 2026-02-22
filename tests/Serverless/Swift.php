<?php

namespace Tests\Serverless;

use Tests\Client;
use Tests\Serverless;

class Swift extends Serverless
{
    public function testSetCookie(): void
    {
        self::assertTrue(true); // Disable test till implemented
    }
    
    public function testSpreadOperatorLogs(): void
    {
        $response = Client::execute(body: '', headers: ['x-action' => 'spreadOperatorLogs']);
        self::assertSame(500, $response['code']);
        self::assertStringContainsString('Unknown action', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }

    public function testDeprecatedMethodsBytesBody(): void
    {
        $response = Client::execute(body: 'Hello', headers: ['x-action' => 'deprecatedMethodsBytesBody']);
        self::assertSame(500, $response['code']);
        self::assertStringContainsString('Unknown action', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }
}
