<?php

namespace Tests\Serverless;

use Tests\Client;
use Tests\Serverless;

class Dart extends Serverless
{
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

    public function testChunkedBinaryRequestBody(): void
    {
        $body = \file_get_contents(__DIR__.'/../resources/large-file-17mb.zip');
        $md5 = \md5($body);

        $response = Client::executeChunked(body: $body, headers: ['x-action' => 'binaryResponseLarge'], method: 'PUT', timeout: 30);
        self::assertEquals(200, $response['code'], $response['error'] ?? '');
        self::assertEquals($md5, $response['body']);
        self::assertEquals('PUT', $response['headers']['x-method']);

        $body = \file_get_contents(__DIR__.'/../resources/large-file-23mb.zip');
        $response = Client::executeChunked(body: $body, headers: ['x-action' => 'binaryResponseLarge'], method: 'PUT', timeout: 30);
        $this->assertThat(
            $response['code'],
            $this->logicalOr(
                $this->equalTo(500),
                $this->equalTo(413),
            ),
        );

        $response = Client::execute(headers: ['x-action' => 'plaintextResponse']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('Hello World 👋', $response['body']);
    }

    public function testConcurrentLargeRequestBodies(): void
    {
        $body = \str_repeat('x', 5 * 1024 * 1024);
        $requests = [];
        for ($i = 0; $i < 3; $i++) {
            $requests[] = [
                'body' => $body,
                'headers' => ['x-action' => 'requestBodyBinary'],
                'method' => 'POST',
                'timeout' => 30,
            ];
        }

        $responses = Client::executeConcurrent($requests);
        self::assertCount(3, $responses);
        foreach ($responses as $response) {
            self::assertEquals(200, $response['code'], $response['error'] ?? '');
            self::assertEquals($body, $response['body']);
        }
    }
}
