<?php

namespace Tests;

class CSR extends Base
{
    public function testHomepage(): void
    {
        $response = Client::execute(url: '/', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertNotEmpty($response['body']);
    }

    public function testFavicon(): void
    {
        $response = Client::execute(url: '/favicon.ico', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertEquals("12e400d2893168e6207185e39df09576", \md5($response['body']));
    }

    public function testStaticFile(): void
    {
        $response = Client::execute(url: '/static.txt', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertEquals('Sample file', $response['body']);
    }

    // TODO: Improve in future, to also ensure build step sees hidden files
    public function testHiddenFile(): void
    {
        $response = Client::execute(url: '/.config/.file', method: 'GET');
        self::assertStringNotContainsString("HIDDEN_FILE", $response['body']);
    }
}
