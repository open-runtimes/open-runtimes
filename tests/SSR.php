<?php

namespace Tests;

class SSR extends CSR
{
    public function testHomepagePrerendered(): void
    {
        $response = Client::execute(url: '/', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString("Hello Open Runtimes", $response['body']);
    }

    public function testServerAction(): void
    {
        $scrapeDate = function (string $body) {
            $date = \explode('[DATE_START]', $body)[1];
            $date = \explode('[DATE_END]', $body)[0];
            return $date;
        };

        $response = Client::execute(url: '/date', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertNotEmpty($response['body']);
        $body1 = $response['body'];
        $date1 = $scrapeDate($body1);

        \sleep(1);

        $response = Client::execute(url: '/date', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertNotEmpty($response['body']);
        $body2 = $response['body'];
        $date2 = $scrapeDate($body2);

        self::assertNotEquals($body1, $body2);
        self::assertNotEquals($date1, $date2);
    }

    public function testServerLogs(): void
    {
        $response = Client::execute(url: '/logs', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString("All logs printed", $response['body']);

        self::assertStringContainsString('A log printed', Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('An error printed', Client::getErrors($response['headers']['x-open-runtimes-log-id']));

        $response = Client::execute(url: '/logs', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString("All logs printed", $response['body']);

        self::assertStringContainsString('A log printed', Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('An error printed', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }

    public function testServerException(): void
    {
        $response = Client::execute(url: '/exception', method: 'GET');
        self::assertEquals(500, $response['code']);
        self::assertStringNotContainsString("No exceptions", $response['body']);
        self::assertStringNotContainsString('Code exception occured', Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('Code exception occured', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }

    public function testServerLibrary(): void
    {
        $scrapeUuid = function (string $body) {
            $date = \explode('[UUID_START]', $body)[1];
            $date = \explode('[UUID_END]', $body)[0];
            return $date;
        };

        $response = Client::execute(url: '/library', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString("My UUID is", $response['body']);
        $uuid1 = $scrapeUuid($response['body']);

        $response = Client::execute(url: '/library', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString("My UUID is", $response['body']);
        $uuid2 = $scrapeUuid($response['body']);

        self::assertNotEquals($uuid1, $uuid2);
    }

    // x-open-runtimes-timeout
    // x-open-runtimes-logging
    // x-open-runtimes-log-id
}
