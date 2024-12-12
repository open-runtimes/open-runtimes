<?php

namespace Tests;

class CSR extends Base
{
    public function setUp(): void
    {
        parent::setUp();

        Client::$port = 3001;
        $this->awaitPortOpen();
        Client::$port = 3000;
    }

    // TODO: Move secret checks to base.php
    public function testWrongSecret(): void
    {
        $response = Client::execute(method: 'GET', url: '/', headers: ['x-open-runtimes-secret' => 'wrongSecret']);
        self::assertEquals(500, $response['code']);
        self::assertEquals('Unauthorized. Provide correct "x-open-runtimes-secret" header.', $response['body']);
    }

    public function testEmptySecret(): void
    {
        $response = Client::execute(method: 'GET', url: '/', headers: ['x-action' => 'plaintextResponse', 'x-open-runtimes-secret' => '']);
        self::assertEquals(500, $response['code']);
        self::assertEquals('Unauthorized. Provide correct "x-open-runtimes-secret" header.', $response['body']);
    }

    public function testEmptyServerSecret(): void
    {
        Client::$port = 3001;

        $response = Client::execute(method: 'GET', url: '/');
        self::assertEquals(200, $response['code']);
        self::assertNotEmpty($response['body']);

        $response = Client::execute(method: 'GET', url: '/', headers: ['x-action' => 'plaintextResponse', 'x-open-runtimes-secret' => 'wrong-secret']);
        self::assertEquals(200, $response['code']);
        self::assertNotEmpty($response['body']);

        Client::$port = 3000;
    }

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
}
