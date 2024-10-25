<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class XStatic extends TestCase
{
    private string $runtimeName = '';
    private string $runtimeVersion = '';
    private string $authHeader = '';
    
    public function setUp(): void
    {
        $this->runtimeName = \getenv('RUNTIME_NAME');
        $this->runtimeVersion = \getenv('RUNTIME_VERSION');

        $this->authHeader = 'Basic ' . \base64_encode('opr:test-secret-key');
    }

    public function tearDown(): void
    {
    }

    public function testAuth(): void
    {
        $response = Client::execute(url: '/', method: 'GET');
        self::assertEquals(401, $response['code']);

        $response = Client::execute(url: '/', method: 'GET', headers: [ 'Authorization' => $this->authHeader ]);
        self::assertEquals(200, $response['code']);   
    }

    public function testFiles(): void
    {
        $response = Client::execute(url: '/index.html', method: 'GET', headers: [ 'Authorization' => $this->authHeader ]);
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website title', $response['body']);
        self::assertStringContainsString('Website body content', $response['body']);
        self::assertStringContainsString('src="main.js"', $response['body']);

        $response = Client::execute(url: '/main.js', method: 'GET', headers: [ 'Authorization' => $this->authHeader ]);
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('JavaScript log', $response['body']);
    }

    public function testCleanUrls(): void
    {
        $response = Client::execute(url: '/contact.html', method: 'GET', headers: [ 'Authorization' => $this->authHeader ]);
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Contact page content', $response['body']);

        $response = Client::execute(url: '/contact', method: 'GET', headers: [ 'Authorization' => $this->authHeader ]);
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Contact page content', $response['body']);

        $response = Client::execute(url: '/about-us/index.html', method: 'GET', headers: [ 'Authorization' => $this->authHeader ]);
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('About us page contents', $response['body']);

        $response = Client::execute(url: '/about-us/', method: 'GET', headers: [ 'Authorization' => $this->authHeader ]);
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('About us page contents', $response['body']);
    }

    public function testFallbackPage() {
        $response = Client::execute(url: '/doesntexist', method: 'GET', headers: [ 'Authorization' => $this->authHeader ]);
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        $response = Client::execute(url: '/doesntexist/', method: 'GET', headers: [ 'Authorization' => $this->authHeader ]);
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);

        $response = Client::execute(url: '/doesntexist.html', method: 'GET', headers: [ 'Authorization' => $this->authHeader ]);
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('Website body content', $response['body']);
    }

    public function testTrailingSlashRedirect()
    {
        $response = Client::execute(url: '/about-us', method: 'GET', headers: [ 'Authorization' => $this->authHeader ]);
        self::assertEquals(308, $response['code']);
        self::assertEquals("/about-us/", $response['headers']['location']);
    }
}
