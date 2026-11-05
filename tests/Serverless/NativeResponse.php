<?php

namespace Tests\Serverless;

use Tests\Client;

/**
 * Shared test for runtimes whose functions may return the native response
 * object of the HTTP library they are built on instead of a context.res.*
 * result: the Fetch API Response in Node.js, Bun and Deno, aiohttp's
 * web.Response in Python and shelf's Response in Dart.
 */
trait NativeResponse
{
    private function skipWithoutNativeResponse(): void
    {
        if ($this->runtimeName === 'node' && \version_compare($this->runtimeVersion, '18.0', '<')) {
            $this->markTestSkipped('The global Response class requires Node.js 18 or newer.');
        }
    }

    public function testNativeResponse(): void
    {
        $this->skipWithoutNativeResponse();

        // Plain text, default status and content-type
        $response = Client::execute(headers: ['x-action' => 'nativeResponseText']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('Hello Native 👋', $response['body']);
        self::assertStringContainsStringIgnoringCase('text/plain', $response['headers']['content-type']);
        self::assertStringContainsStringIgnoringCase('charset=utf-8', $response['headers']['content-type']);
        self::assertNotEmpty($response['headers']['x-open-runtimes-log-id']);

        // JSON body with custom status
        $response = Client::execute(headers: ['x-action' => 'nativeResponseJson']);
        self::assertEquals(201, $response['code']);
        self::assertEqualsIgnoringWhitespace('application/json; charset=utf-8', $response['headers']['content-type']);
        $body = \json_decode($response['body'], true);
        self::assertEquals(true, $body['json']);
        self::assertEquals('Native responses are awesome.', $body['message']);

        // Custom headers are forwarded, reserved headers are stripped
        $response = Client::execute(headers: ['x-action' => 'nativeResponseHeaders']);
        self::assertEquals(404, $response['code']);
        self::assertEquals('OK', $response['body']);
        self::assertEquals('first-value', $response['headers']['first-header']);
        self::assertArrayNotHasKey('x-open-runtimes-custom-out-header', $response['headers']);

        // Binary body
        $response = Client::execute(headers: ['x-action' => 'nativeResponseBinary']);
        self::assertEquals(200, $response['code']);
        self::assertStringContainsStringIgnoringCase('application/octet-stream', $response['headers']['content-type']);
        $bytes = \unpack('C*byte', $response['body']);
        self::assertCount(3, $bytes);
        self::assertEquals(0, $bytes['byte1']);
        self::assertEquals(10, $bytes['byte2']);
        self::assertEquals(255, $bytes['byte3']);

        // Empty body
        $response = Client::execute(headers: ['x-action' => 'nativeResponseEmpty']);
        self::assertEquals(204, $response['code']);
        self::assertEmpty($response['body']);

        // Response.redirect()
        $response = Client::execute(headers: ['x-action' => 'nativeResponseRedirect']);
        self::assertEquals(301, $response['code']);
        self::assertEmpty($response['body']);
        self::assertEquals('https://github.com/', $response['headers']['location']);
    }

    public function testNativeResponseStream(): void
    {
        $this->skipWithoutNativeResponse();

        // Streamed body is fully consumed
        $response = Client::execute(headers: ['x-action' => 'nativeResponseStream']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('Hello Stream', $response['body']);
    }

    public function testNativeResponseChunks(): void
    {
        if (!\in_array($this->runtimeName, ['node', 'bun', 'deno'])) {
            $this->markTestSkipped('Only the JavaScript runtimes stream a native response body.');
        }
        $this->skipWithoutNativeResponse();

        $start = \microtime(true);
        $received = '';
        $arrivals = [];
        $response = Client::execute(headers: ['x-action' => 'nativeResponseChunks'], timeout: 10, bodyCallback: function (string $chunk) use (&$received, &$arrivals) {
            $received .= $chunk;
            while (\count($arrivals) < \substr_count($received, "\n")) {
                $arrivals[] = \microtime(true);
            }
        });

        self::assertEquals(201, $response['code']);
        self::assertEquals('yes', $response['headers']['x-stream']);
        self::assertEquals("first\nsecond\nthird\n", $response['body']);
        self::assertLessThan(0.5, $arrivals[0] - $start);
        self::assertEqualsWithDelta(1, $arrivals[1] - $arrivals[0], 0.4);
        self::assertEqualsWithDelta(1, $arrivals[2] - $arrivals[1], 0.4);
    }
}
