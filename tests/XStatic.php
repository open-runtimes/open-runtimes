<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class XStatic extends Base
{
    public function testCacheHeader(): void
    {
        
        $response = Client::execute(url: '/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertArrayHasKey('cdn-cache-control', $response['headers']);
        self::assertArrayNotHasKey('surrogate-control', $response['headers']);
        self::assertEquals('public, max-age=36000', $response['headers']['cdn-cache-control']);
        
        $response = Client::execute(url: '/some/nested/path.txt', method: 'GET');
        self::assertArrayHasKey('cdn-cache-control', $response['headers']);
        self::assertArrayNotHasKey('surrogate-control', $response['headers']);
        
        Client::$host = 'open-runtimes-test-serve-secondary';
        
        $response = Client::execute(url: '/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertArrayNotHasKey('cdn-cache-control', $response['headers']);
        self::assertArrayHasKey('surrogate-control', $response['headers']);
        self::assertEquals('public, max-age=36000', $response['headers']['surrogate-control']);
        
        $response = Client::execute(url: '/some/nested/path.txt', method: 'GET');
        self::assertArrayNotHasKey('cdn-cache-control', $response['headers']);
        self::assertArrayHasKey('surrogate-control', $response['headers']);
        
        Client::$host = 'open-runtimes-test-serve';
    }
    
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

        Client::$host = 'open-runtimes-test-serve-secondary';

        $response = Client::execute(url: '/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        $response = Client::execute(url: '/missing.html', method: 'GET');
        self::assertEquals(404, $response['code']);
        self::assertStringContainsString('404', $response['body']);
        self::assertStringContainsString('Page not found', $response['body']);
        self::assertStringNotContainsString('Website body content', $response['body']);

        Client::$host = 'open-runtimes-test-serve-teritary';

        $response = Client::execute(url: '/index.html', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        $response = Client::execute(url: '/missing.html', method: 'GET');
        self::assertEquals(404, $response['code']);
        self::assertStringContainsString('Custom-branded 404 page', $response['body']);
        self::assertStringNotContainsString('Website body content', $response['body']);

        Client::$host = 'open-runtimes-test-serve';
    }

    // TODO: Improve in future, to also ensure build step sees hidden files
    public function testHiddenFile(): void
    {
         Client::$host = 'open-runtimes-test-serve-secondary';
         
        $response = Client::execute(url: '/.config/.file', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('VISIBLE=yes', $response['body']);

        $response = Client::execute(url: '/.hidden', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('VISIBLE=yes', $response['body']);

        $response = Client::execute(url: '/.gitignore', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('!.env', $response['body']);

        $response = Client::execute(url: '/.well-known/apple-app-site-association', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('ABCDE12345.com.example.myapp', $response['body']);

        $response = Client::execute(url: '/.well-known/assetlinks.json', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('com.example.myapp', $response['body']);

        $response = Client::execute(url: '/.env', method: 'GET');
        self::assertEquals(404, $response['code']);
        self::assertStringContainsString('Page not found', $response['body']);

        $response = Client::execute(url: '/.env.dev', method: 'GET');
        self::assertEquals(404, $response['code']);
        self::assertStringContainsString('Page not found', $response['body']);

        $response = Client::execute(url: '/.env.anysuffix', method: 'GET');
        self::assertEquals(404, $response['code']);
        self::assertStringContainsString('Page not found', $response['body']);

        $response = Client::execute(url: '/.config/.env', method: 'GET');
        self::assertEquals(404, $response['code']);
        self::assertStringContainsString('Page not found', $response['body']);
        
        Client::$host = 'open-runtimes-test-serve';
    }
}
