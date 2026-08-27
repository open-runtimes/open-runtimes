<?php

namespace Tests\SSR;

use Tests\Client;
use Tests\SSR;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class Jaspr extends SSR
{
    public function testHomepagePrerendered(): void
    {
        $response = Client::execute(url: '/', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString("hello_jaspr", $response['body']);
    }

    public function testServerAction(): void
    {
        $this->markTestSkipped('Jaspr fixture has no /date route yet');
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

    public function testServerException(): void
    {
        $this->markTestSkipped('Jaspr fixture has no /exception route yet');
    }

    public function testServerLibrary(): void
    {
        $this->markTestSkipped('Jaspr fixture has no /library route yet');
    }

    public function testHiddenFile(): void
    {
        $this->markTestSkipped('Jaspr fixture has no /hidden route yet');
    }

    public function testModclean(): void
    {
        $this->markTestSkipped('Dart builds have no node_modules to prune');
    }

    public function testNft(): void
    {
        $this->markTestSkipped('Dart builds have no node_modules to prune');
    }

    public function testStaticCache(): void
    {
        $this->markTestSkipped('Jaspr helper does not set cache-control headers on static assets yet');
    }
}
