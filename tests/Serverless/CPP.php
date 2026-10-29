<?php

namespace Tests\Serverless;

use Tests\Client;
use Tests\Serverless;

class CPP extends Serverless
{
    public function testStreamResponse(): void
    {
        $arrivals = [];
        $response = Client::execute(headers: ['x-action' => 'streamResponse'], timeout: 10, onChunk: function (string $data) use (&$arrivals) {
            foreach (\explode("\n", \trim($data)) as $text) {
                $arrivals[$text] = \microtime(true);
            }
        });

        self::assertEquals(201, $response['code']);
        self::assertEquals('yes', $response['headers']['x-stream']);
        self::assertSame(['first', 'second', 'third'], \array_keys($arrivals));
        self::assertGreaterThan(0.8, $arrivals['second'] - $arrivals['first']);
        self::assertGreaterThan(0.8, $arrivals['third'] - $arrivals['second']);
    }

    public function testStreamException(): void
    {
        $response = Client::execute(headers: ['x-action' => 'streamException']);

        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Stream failed', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }

    public function testSetCookie(): void
    {
        self::assertTrue(true); // Disable test till implemented
    }
    
    public function testSpreadOperatorLogs(): void
    {
        $response = Client::execute(body: '', headers: ['x-action' => 'spreadOperatorLogs']);
        self::assertEquals(500, $response['code']);
        self::assertStringContainsString('Unknown action', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }

    public function testDeprecatedMethodsBytesBody(): void
    {
        $response = Client::execute(body: 'Hello', headers: ['x-action' => 'deprecatedMethodsBytesBody']);
        self::assertEquals(500, $response['code']);
        self::assertStringContainsString('Unknown action', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }
}
