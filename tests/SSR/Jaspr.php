<?php

namespace Tests\SSR;

use Tests\Client;
use Tests\SSR;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class Jaspr extends SSR
{
    // Jaspr compiles to a standalone AOT binary, so it can't be hooked into the
    // open-runtimes log/error capture contract the way Node's NODE_OPTIONS import
    // wraps http.createServer. So this only checks what doesn't depend on that:
    // the route exists, throws, and returns 500 without leaking "No exceptions".
    public function testServerException(): void
    {
        $response = Client::execute(url: '/exception', method: 'GET');
        self::assertEquals(500, $response['code']);
        self::assertStringNotContainsString("No exceptions", $response['body']);
    }

    public function testServerLogs(): void
    {
        $this->markTestSkipped('Jaspr SSR compiles to native Dart AOT — stdout capture not supported yet');
    }

    public function testServerLogsConcurrency(): void
    {
        $this->markTestSkipped('Jaspr SSR compiles to native Dart AOT — stdout capture not supported yet');
    }

    public function testDevLogFiles(): void
    {
        $this->markTestSkipped('Jaspr SSR compiles to native Dart AOT — stdout capture not supported yet');
    }

    public function testHiddenFile(): void
    {
        $response = Client::execute(url: '/hidden', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('HIDDEN_FILE', $response['body']);
    }

    public function testModclean(): void
    {
        $this->markTestSkipped('Dart builds have no node_modules to prune');
    }

    public function testNft(): void
    {
        $this->markTestSkipped('Dart builds have no node_modules to prune');
    }
}
