<?php

namespace Tests\Serverless;

use Tests\Client;
use Tests\Serverless;

class Bun extends Serverless
{
    public function testHeadlessBrowser(): void
    {
        $response = Client::execute(body: '', headers: ['x-action' => 'headlessBrowser'], timeout: 15);
        
        \var_dump($response['code']);
        \var_dump($response['headers']);
        \var_dump(Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        \var_dump(Client::getErrors($response['headers']['x-open-runtimes-log-id']));
        
        self::assertEquals(200, $response['code']);
        self::assertEquals('image/png; charset=utf-8', $response['headers']['content-type']);
        self::assertGreaterThanOrEqual(100000, \mb_strlen($response['body'])); // Should be 1.355MB
    }
}
