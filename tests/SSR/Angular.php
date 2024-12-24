<?php

namespace Tests\SSR;

use Tests\Client;
use Tests\SSR;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class Angular extends SSR
{
    // Override because Angular uses island architecture, so parts of website were successfully rendered
    public function testServerException(): void
    {
        $response = Client::execute(url: '/exception', method: 'GET');
        self::assertEquals(200, $response['code']); // Overriden from assertion 500
        self::assertStringContainsString("<router-outlet></router-outlet>", $response['body']); // New assertion
        self::assertStringNotContainsString("No exceptions", $response['body']);
        self::assertStringNotContainsString('Code exception occured', Client::getLogs('ssr'));
        self::assertStringContainsString('Code exception occured', Client::getErrors('ssr'));
    }
}
