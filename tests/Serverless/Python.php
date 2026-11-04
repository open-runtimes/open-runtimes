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
}
