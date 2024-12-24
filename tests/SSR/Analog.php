<?php

namespace Tests\SSR;

use Tests\Client;
use Tests\SSR;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class Analog extends SSR
{
    // Override because Angular uses island architecture, so parts of website were successfully rendered
    // Analog also doesnt print error message
    public function testServerException(): void
    {
        $response = Client::execute(url: '/exception', method: 'GET');
        self::assertEquals(200, $response['code']); // Changed
        self::assertStringContainsString("<router-outlet></router-outlet>", $response['body']); // Added
        self::assertStringNotContainsString("No exceptions", $response['body']);
        self::assertStringNotContainsString('Code exception occured', Client::getLogs('ssr'));
        self::assertStringNotContainsString('ERROR Error: NG04002: \'exception\'', Client::getLogs('ssr')); // Added
        self::assertStringContainsString('ERROR Error: NG04002: \'exception\'', Client::getErrors('ssr')); // Changed
    }
}
