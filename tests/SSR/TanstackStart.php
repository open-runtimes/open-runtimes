<?php

namespace Tests\SSR;

use Tests\Client;
use Tests\SSR;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class TanstackStart extends SSR
{
    // Tanstack Start does not print exception as logs, instead, it goest to error page renderer
    public function testServerException(): void
    {
        $response = Client::execute(url: '/exception', method: 'GET');
        self::assertEquals(500, $response['code']);
        self::assertStringNotContainsString("No exceptions", $response['body']);
        self::assertStringNotContainsString('Code exception occured', Client::getLogs($response['headers']['x-open-runtimes-log-id']));

        // NOT in server error logs
        self::assertStringNotContainsString('Code exception occured', Client::getErrors($response['headers']['x-open-runtimes-log-id']));

        // YES in response body
        self::assertStringContainsString("Code exception occured", $response['body']);
    }
}
