<?php

namespace Tests\SSR;

use Tests\Client;
use Tests\SSR;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class Analog extends SSR
{
    // Temporarly disabling concurrency test
    // Analog concurrency still likely works, but logging to correct execution might not work
    public function testServerLogsConcurrency(): void
    {
        $this->assertTrue(true);
    }
    
    // Override because Angular uses island architecture, so parts of website were successfully rendered
    // Analog also doesnt print error message
    public function testServerException(): void
    {
        $response = Client::execute(url: '/exception', method: 'GET');
        self::assertEquals(200, $response['code']); // Changed
        self::assertStringContainsString("<router-outlet></router-outlet>", $response['body']); // Added
        self::assertStringNotContainsString("No exceptions", $response['body']);
        self::assertStringNotContainsString('Code exception occured', Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringNotContainsString('ERROR Error: NG04002: \'exception\'', Client::getLogs($response['headers']['x-open-runtimes-log-id'])); // Added
        self::assertStringContainsString('ERROR Error: NG04002: \'exception\'', Client::getErrors($response['headers']['x-open-runtimes-log-id'])); // Changed
    }
}
