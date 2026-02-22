<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class Base extends TestCase
{
    protected string $runtimeName = '';
    protected string $runtimeVersion = '';

    public function setUp(): void
    {
        $this->runtimeName = \getenv('RUNTIME_NAME');
        $this->runtimeVersion = \getenv('RUNTIME_VERSION');

        Client::$secret = \getenv('OPEN_RUNTIMES_SECRET');

        Client::$host = 'open-runtimes-test-serve';
        $this->awaitHostReady();
        Client::$host = 'open-runtimes-test-serve-secondary';
        $this->awaitHostReady();
        Client::$host = 'open-runtimes-test-serve-teritary';
        $this->awaitHostReady();
        Client::$host = 'open-runtimes-test-serve';
    }

    public function tearDown(): void
    {
    }

    protected function awaitHostReady() {
        $attempts = 0;
        while(true) {
            $response = Client::execute();
            if($response['code'] != 0) {
                return;
            }

            if($attempts >= 100) {
                break;
            }

            sleep(1);
            $attempts++;
        }

        // Getting here means timeout failure
        throw new \Exception("Server did not start on port :3000 within 100 seconds. Check docker container logs");
    }

    public function testWrongSecret(): void
    {
        Client::$secret = 'wrongSecret';

        $response = Client::execute(method: 'GET', url: '/', headers: ['x-action' => 'plaintextResponse']);
        $this->assertThat(
            $response['code'],
            $this->logicalOr(
                $this->equalTo(500),
                $this->equalTo(401),
            ),
        );
        self::assertStringContainsString('Unauthorized', $response['body']);

        Client::$secret = \getenv('OPEN_RUNTIMES_SECRET');
    }

    public function testEmptySecret(): void
    {
        Client::$secret = '';

        $response = Client::execute(method: 'GET', url: '/', headers: ['x-action' => 'plaintextResponse']);
        $this->assertThat(
            $response['code'],
            $this->logicalOr(
                $this->equalTo(500),
                $this->equalTo(401),
            ),
        );
        self::assertStringContainsString('Unauthorized', $response['body']);

        Client::$secret = \getenv('OPEN_RUNTIMES_SECRET');
    }

    public function testEmptyServerSecret(): void
    {
        Client::$host = 'open-runtimes-test-serve-secondary';

        $response = Client::execute(method: 'GET', url: '/', headers: ['x-action' => 'plaintextResponse']);
        self::assertSame(200, $response['code']);
        self::assertNotEmpty($response['body']);

        Client::$secret = 'wrongSecret';

        $response = Client::execute(method: 'GET', url: '/', headers: ['x-action' => 'plaintextResponse', 'x-open-runtimes-secret' => 'wrong-secret']);
        self::assertSame(200, $response['code']);
        self::assertNotEmpty($response['body']);

        Client::$secret = \getenv('OPEN_RUNTIMES_SECRET');
        Client::$host = 'open-runtimes-test-serve';
    }

    public function testHealth(): void
    {
        $response = Client::execute(method: 'GET', url: '/__opr/health');
        self::assertSame(200, $response['code']);
        self::assertSame('OK', $response['body']);
    }

    public function testTimings(): void
    {
        $response = Client::execute(method: 'GET', url: '/__opr/timings');
        self::assertSame(200, $response['code']);
        self::assertStringContainsString('text/plain', $response['headers']['content-type']);

        // Parse key=value format
        $timings = [];
        foreach (explode("\n", trim($response['body'])) as $line) {
            if (empty($line)) continue;
            [$key, $value] = explode('=', $line, 2);
            $timings[$key] = (float)$value;
        }

        // Timings should contain existing data from mounted file
        self::assertArrayHasKey('local_download', $timings);
        self::assertIsFloat($timings['local_download']);
        self::assertArrayHasKey('remote_download', $timings);
        self::assertIsFloat($timings['remote_download']);
        self::assertArrayHasKey('extract', $timings);
        self::assertIsFloat($timings['extract']);
    }
}
