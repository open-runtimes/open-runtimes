<?php

namespace Tests\Serverless;

use Tests\Client;
use Tests\Serverless;

abstract class Javascript extends Serverless
{
    public function testRawResponse(): void
    {
        if (str_starts_with($this->runtimeVersion, '16.0')) {
            self::markTestSkipped('Node 16 does not support raw response object.');
        }

        // Text response
        $response = Client::execute(headers: ['x-action' => 'responseObjectText']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('Hello World 👋', $response['body']);
        self::assertEqualsIgnoringWhitespace('text/plain; charset=utf-8', $response['headers']['content-type']);

        // JSON response
        $response = Client::execute(headers: ['x-action' => 'responseObjectJson']);
        self::assertEquals(200, $response['code']);
        self::assertEqualsIgnoringWhitespace('application/json; charset=utf-8', $response['headers']['content-type']);

        $body = \json_decode($response['body'], true);
        self::assertEquals(true, $body['json']);
        self::assertEquals('Developers are awesome.', $body['message']);

        // Binary response
        $response = Client::execute(headers: ['x-action' => 'responseObjectBinary']);
        self::assertEquals(200, $response['code']);
        $bytes = \unpack('C*byte', $response['body']);
        self::assertCount(3, $bytes);
        self::assertEquals(0, $bytes['byte1']);
        self::assertEquals(10, $bytes['byte2']);
        self::assertEquals(255, $bytes['byte3']);

        // Empty response
        $response = Client::execute(headers: ['x-action' => 'responseObjectEmpty']);
        self::assertEquals(204, $response['code']);
        self::assertEmpty($response['body']);

        // Redirect response
        $response = Client::execute(headers: ['x-action' => 'responseObjectRedirect']);
        self::assertEquals(301, $response['code']);
        self::assertEmpty($response['body']);
        self::assertEquals('https://github.com/', $response['headers']['location']);

        // Custom status code
        $response = Client::execute(headers: ['x-action' => 'responseObjectStatus']);
        self::assertEquals(404, $response['code']);
        self::assertEquals('FAIL', $response['body']);

        // Custom headers
        $response = Client::execute(headers: ['x-action' => 'responseObjectHeaders']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('OK', $response['body']);
        self::assertEquals('first-value', $response['headers']['first-header']);
        self::assertEquals('second-value', $response['headers']['second-header']);
    }

    public function testStreamingResponse(): void
    {
        if (str_starts_with($this->runtimeVersion, '16.0')) {
            self::markTestSkipped('Node 16 does not support raw response object.');
        }

        $response = Client::executeStreaming(headers: ['x-action' => 'streamingResponse']);

        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('chunk1', $response['body']);
        self::assertStringContainsString('chunk2', $response['body']);
        self::assertEquals('chunk1chunk2', $response['body']);

        // Prove streaming: chunks must arrive with a time gap
        self::assertGreaterThanOrEqual(2, \count($response['chunks']));
        $firstTime = $response['chunks'][0]['time'];
        $lastTime = $response['chunks'][\count($response['chunks']) - 1]['time'];
        self::assertGreaterThanOrEqual(0.5, $lastTime - $firstTime,
            'Chunks should arrive with >= 0.5s gap, proving streaming not buffering');
    }

    public function testRawRequest(): void
    {
        if (str_starts_with($this->runtimeVersion, '16.0')) {
            self::markTestSkipped('Node 16 does not support raw request object.');
        }

        $response = Client::execute(
            body: 'Hello Native',
            method: 'POST',
            headers: ['x-action' => 'rawRequest', 'content-type' => 'text/plain']
        );
        self::assertEquals(200, $response['code']);

        $body = \json_decode($response['body'], true);

        self::assertEquals('POST', $body['method']);
        self::assertEquals('Hello Native', $body['body']);
        self::assertTrue($body['hasContentType']);
        self::assertStringStartsWith('http', $body['url']);
    }
}