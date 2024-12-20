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
        // Helper for scraping date from the body. Possibly containing irelevant non-dynamic extras.
        // Do not use with Date or DateTime constructors.
        $scrapeDate = function (string $body) {
            // <p id="date">CONTENT</p>
            $date = \explode('id="date"', $body)[1];
            $date = \explode('</p>', $body)[0];
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

        self::assertStringContainsString('A log printed', Client::getLogs('ssr'));
        self::assertStringContainsString('An error printed', Client::getErrors('ssr'));

        $response = Client::execute(url: '/logs', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString("All logs printed", $response['body']);

        self::assertEquals(2, \substr_count(Client::getLogs('ssr'), 'A log printed'));
        self::assertEquals(2, \substr_count(Client::getErrors('ssr'), 'An error printed'));
    }

    public function testServerException(): void
    {
        $response = Client::execute(url: '/exception', method: 'GET');
        self::assertEquals(500, $response['code']);
        self::assertStringNotContainsString("No exceptions", $response['body']);
        self::assertStringNotContainsString('Code exception occured', Client::getLogs('ssr'));
        self::assertStringContainsString('Code exception occured', Client::getErrors('ssr'));
    }
}
