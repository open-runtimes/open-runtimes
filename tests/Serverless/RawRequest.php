<?php

namespace Tests\Serverless;

use Tests\Client;

/**
 * Shared test for runtimes that expose the incoming request as a native
 * (Fetch API) Request through context.req.raw. Used by the JavaScript
 * runtimes (Node.js, Bun, Deno).
 */
trait RawRequest
{
    public function testRawRequest(): void
    {
        if ($this->runtimeName === 'node' && \version_compare($this->runtimeVersion, '18.0', '<')) {
            $this->markTestSkipped('The global Request class requires Node.js 18 or newer.');
        }

        // Text body, headers, URL and identity
        $response = Client::execute(body: 'Hello raw 👋', headers: ['x-action' => 'rawRequest', 'x-first-header' => 'first-value', 'x-open-runtimes-custom-header' => 'should-be-hidden']);
        self::assertEquals(200, $response['code'], $response['body']);
        $body = \json_decode($response['body'], true);
        self::assertTrue($body['isRequest']);
        self::assertTrue($body['isMemoized']);
        self::assertEquals('POST', $body['method']);
        self::assertEquals($body['contextUrl'], $body['url']);
        self::assertContains($body['url'], ['http://localhost:3000/', 'http://0.0.0.0:3000/', 'http://127.0.0.1:3000/', 'http://open-runtimes-test-serve-secondary:3000/', 'http://open-runtimes-test-serve:3000/']);
        self::assertEquals('Hello raw 👋', $body['bodyText']);
        self::assertEquals('first-value', $body['headers']['x-first-header']);
        self::assertEquals('rawRequest', $body['headers']['x-action']);
        self::assertStringStartsWith('text/plain', $body['headers']['content-type']);
        self::assertArrayNotHasKey('x-open-runtimes-custom-header', $body['headers']);
        self::assertArrayNotHasKey('x-open-runtimes-secret', $body['headers']);

        // JSON body read through the native Request
        $payload = ['name' => 'Open Runtimes', 'version' => 3.5, 'published' => true, 'nested' => [['object' => 1]]];
        $response = Client::execute(body: \json_encode($payload), headers: ['x-action' => 'rawRequestJson', 'content-type' => 'application/json']);
        self::assertEquals(200, $response['code'], $response['body']);
        self::assertEquals($payload, \json_decode($response['body'], true));

        // GET request has no body
        $response = Client::execute(method: 'GET', headers: ['x-action' => 'rawRequestEmpty']);
        self::assertEquals(200, $response['code'], $response['body']);
        $body = \json_decode($response['body'], true);
        self::assertEquals('GET', $body['method']);
        self::assertFalse($body['hasBody']);

        // Methods the Fetch API forbids cannot be represented; expect a descriptive error
        $response = Client::execute(method: 'TRACE', headers: ['x-action' => 'rawRequestEmpty']);
        self::assertEquals(500, $response['code']);
        self::assertStringContainsString('context.req.raw is not available for TRACE requests', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }
}
