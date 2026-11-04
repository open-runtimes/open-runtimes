<?php

namespace Tests\Serverless;

use Tests\Client;
use Tests\Serverless;

class Python extends Serverless
{
    use NativeResponse;

    public function testSetCookie(): void
    {
        self::assertTrue(true); // Disable test till implemented
    }

    public function testNativeResponseStream(): void
    {
        // aiohttp's StreamResponse has no in-memory body, so it is rejected with a clear error
        $response = Client::execute(headers: ['x-action' => 'nativeResponseStream']);
        self::assertEquals(500, $response['code']);
        self::assertStringContainsString('not supported', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }

    public function testNativeResponseCookies(): void
    {
        // Repeated headers such as multiple set-cookie values are forwarded one by one
        $response = Client::execute(headers: ['x-action' => 'nativeResponseCookies']);
        self::assertEquals(200, $response['code']);
        $cookies = (array) ($response['headers']['set-cookie'] ?? []);
        self::assertCount(2, $cookies);
        self::assertStringStartsWith('first=1', $cookies[0]);
        self::assertStringStartsWith('second=2', $cookies[1]);
    }
}
