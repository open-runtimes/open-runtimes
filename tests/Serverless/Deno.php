<?php

namespace Tests\Serverless;

use Tests\Client;
use Tests\Serverless;

class Deno extends Serverless
{
    public function testHeadlessBrowser(): void
    {
        $response = Client::execute(body: '', headers: ['x-action' => 'headlessBrowser'], timeout: 15);
        self::assertEquals(200, $response['code']);
        self::assertEquals('image/png; charset=utf-8', $response['headers']['content-type']);
        self::assertGreaterThanOrEqual(100000, \mb_strlen($response['body'])); // Should be 1.355MB
    }
}
