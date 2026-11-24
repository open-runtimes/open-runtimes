<?php

namespace Tests\Serverless;

use Tests\Client;

/**
 * Shared test for runtimes that publish the request being served under
 * Symbol.for("open-runtimes.request-context"), so a library the app bundles
 * can read it without being handed the context. Used by the Node.js and Bun
 * runtimes, and by Express for the SSR runtime.
 */
trait RequestContext
{
    public function testRequestContext(): void
    {
        // Headers of the request being served, before and after an await
        $response = Client::execute(headers: ['x-action' => 'requestContext', 'x-first-header' => 'first-value', 'x-open-runtimes-custom-header' => 'should-be-hidden']);
        self::assertEquals(200, $response['code'], $response['body']);
        $body = \json_decode($response['body'], true);
        self::assertEquals('first-value', $body['before']['x-first-header']);
        self::assertEquals('requestContext', $body['before']['x-action']);
        self::assertEquals($body['before'], $body['after']);
        self::assertArrayNotHasKey('x-open-runtimes-custom-header', $body['before']);
        self::assertArrayNotHasKey('x-open-runtimes-secret', $body['before']);

        // A streamed response still reads it for the chunks sent after the first
        $response = Client::execute(headers: ['x-action' => 'requestContextStream', 'x-first-header' => 'first-value'], bodyCallback: fn () => null);
        self::assertEquals(200, $response['code']);
        self::assertEquals("first-value\nfirst-value\n", $response['body']);

        // Overlapping requests each read their own
        $requests = [];
        foreach (['slow' => '500', 'fast' => '0'] as $name => $delay) {
            $requests[] = [
                'headers' => ['x-action' => 'requestContext', 'x-first-header' => $name, 'x-delay' => $delay],
            ];
        }

        $responses = Client::executeConcurrent($requests);
        self::assertCount(2, $responses);
        foreach (['slow', 'fast'] as $index => $name) {
            self::assertEquals(200, $responses[$index]['code'], $responses[$index]['error'] ?? '');
            $body = \json_decode($responses[$index]['body'], true);
            self::assertEquals($name, $body['before']['x-first-header']);
            self::assertEquals($name, $body['after']['x-first-header']);
        }
    }
}
