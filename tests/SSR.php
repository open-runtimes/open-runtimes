<?php

namespace Tests;

class SSR extends CSR
{
    public function setUp(): void
    {
        parent::setUp();
    }

    public function testHomepagePrerendered(): void
    {
        $response = Client::execute(url: '/', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString("Hello Open Runtimes", $response['body']);
    }

    public function testServerAction(): void
    {
        $response = Client::execute(url: '/date', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertNotEmpty($response['body']);
        $date1 = $response['body'];

        \sleep(1);

        $response = Client::execute(url: '/date', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertNotEmpty($response['body']);
        $date2 = $response['body'];

        self::assertNotEquals($date1, $date2);
    }
}
