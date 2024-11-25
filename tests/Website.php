<?php

namespace Tests;

class Website extends Base
{
    public function setUp(): void
    {
        parent::setUp();
    }

    public function testStaticFile(): void
    {
        $response = Client::execute(url: '/', method: 'GET');
        self::assertEquals(200, $response['code']);

        $response = Client::execute(url: '/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);

        $response = Client::execute(url: '/favicon.png', method: 'GET');
        self::assertEquals(200, $response['code']);

        $response = Client::execute(url: '/static.txt', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertEquals('Sample file', $response['body']);
    }
}
