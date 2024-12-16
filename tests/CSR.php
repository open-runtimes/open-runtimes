<?php

namespace Tests;

class CSR extends Base
{
    public function setUp(): void
    {
        parent::setUp();
    }

    public function testHomepage(): void
    {
        // We do not test response body on purpose; HTML is not always pre-rendered (empty <body> until JS runs)

        $response = Client::execute(url: '/', method: 'GET');
        self::assertEquals(200, $response['code']);
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
}
