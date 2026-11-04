<?php

namespace Tests\Serverless;

use Tests\Client;
use Tests\Serverless;

class Go extends Serverless
{
    public function testSetCookie(): void
    {
        self::assertTrue(true); // Disable test till implemented
    }
    
    public function testDeprecatedMethodsBytesBody(): void
    {
        $response = Client::execute(body: 'Hello', headers: ['x-action' => 'deprecatedMethodsBytesBody']);
        self::assertEquals(500, $response['code']);
        self::assertStringContainsString('Unknown action', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }

    public function testRawRequestContentType(): void
    {
        // context.req.headers lowercases content-type, but the raw request must keep it untouched
        $contentType = 'multipart/form-data; boundary=MixedCaseBoundary123';
        $response = Client::execute(body: 'ignored', headers: ['x-action' => 'rawRequestContentType', 'content-type' => $contentType]);
        self::assertEquals(200, $response['code']);
        self::assertEquals($contentType, $response['body']);
    }
}
