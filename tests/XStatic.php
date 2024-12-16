<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class XStatic extends Base
{
    public function testFiles(): void
    {
        $response = Client::execute(url: '/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website title', $response['body']);
        self::assertStringContainsString('Website body content', $response['body']);
        self::assertStringContainsString('src="main.js"', $response['body']);

        $response = Client::execute(url: '/main.js', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('JavaScript log', $response['body']);
    }

    public function testCleanUrls(): void
    {
        $response = Client::execute(url: '/contact.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Contact page content', $response['body']);

        $response = Client::execute(url: '/contact', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Contact page content', $response['body']);

        $response = Client::execute(url: '/about-us/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('About us page contents', $response['body']);

        $response = Client::execute(url: '/about-us/', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('About us page contents', $response['body']);
    }

    public function testFallbackPage() {
        $response = Client::execute(url: '/doesntexist', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        $response = Client::execute(url: '/doesntexist/', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        $response = Client::execute(url: '/doesntexist.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);
    }

    public function testTrailingSlashRedirect()
    {
        $response = Client::execute(url: '/about-us', method: 'GET');
        self::assertEquals(308, $response['code']);
        self::assertEquals("/about-us/", $response['headers']['location']);
    }

    public function testFallbackFile()
    {
        $response = Client::execute(url: '/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        $response = Client::execute(url: '/missing.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        Client::$port = 3001;

        $response = Client::execute(url: '/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        $response = Client::execute(url: '/missing.html', method: 'GET');
        self::assertEquals(404, $response['code']);
        self::assertStringContainsString('404', $response['body']);
        self::assertStringContainsString('Page not found', $response['body']);
        self::assertStringNotContainsString('Website body content', $response['body']);

        Client::$port = 3002;

        $response = Client::execute(url: '/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        $response = Client::execute(url: '/missing.html', method: 'GET');
        self::assertEquals(404, $response['code']);
        self::assertStringContainsString('Custom-branded 404 page', $response['body']);
        self::assertStringNotContainsString('Website body content', $response['body']);

        Client::$port = 3000;
    }
}
