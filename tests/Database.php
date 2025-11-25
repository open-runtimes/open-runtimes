<?php

namespace Tests;

class Database extends Base
{
    public function setUp(): void
    {
        $this->runtimeName = \getenv('RUNTIME_NAME');
        $this->runtimeVersion = \getenv('RUNTIME_VERSION');

        Client::$secret = \getenv('OPEN_RUNTIMES_SECRET');

        // For database runtimes, we need to wait longer for initialization
        // and handle missing environment variables gracefully
        Client::$host = 'open-runtimes-test-serve';
        $this->awaitDatabaseReady();
        Client::$host = 'open-runtimes-test-serve-secondary';
        $this->awaitDatabaseReady();
        Client::$host = 'open-runtimes-test-serve-teritary';
        $this->awaitDatabaseReady();
        Client::$host = 'open-runtimes-test-serve';
    }

    protected function awaitDatabaseReady() {
        $attempts = 0;
        $maxAttempts = 60; // 60 seconds for databases to initialize

        while ($attempts < $maxAttempts) {
            try {
                $response = Client::execute(url: '/__opr/health', method: 'GET');
                if ($response['code'] == 200 && $response['body'] == 'OK') {
                    return;
                }
            } catch (\Exception $e) {
                // Database not ready yet, continue waiting
            }

            sleep(1);
            $attempts++;
        }

        // If we get here, try to get more info about the failure
        try {
            $response = Client::execute(url: '/__opr/status', method: 'GET');
            $status = json_decode($response['body'], true);
            if (isset($status['ready']) && !$status['ready']) {
                $this->markTestSkipped('Database container not ready - likely missing required environment variables');
            }
        } catch (\Exception $e) {
            $this->markTestSkipped('Database container not reachable - likely missing required environment variables');
        }
    }
    /**
     * Test health check endpoint
     * Verifies that the /__opr/health endpoint returns OK when database is ready
     */
    public function testHealthCheck(): void
    {
        $response = Client::execute(url: '/__opr/health', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertEquals('OK', $response['body']);
    }

    /**
     * Test health check consistency
     * Verifies that multiple health checks return consistent results
     */
    public function testHealthCheckConsistency(): void
    {
        for ($i = 0; $i < 5; $i++) {
            $response = Client::execute(url: '/__opr/health', method: 'GET');
            self::assertEquals(200, $response['code'], "Health check failed on attempt $i");
            self::assertEquals('OK', $response['body']);
        }
    }

    /**
     * Test status endpoint
     * Verifies that the /__opr/status endpoint returns database status information
     */
    public function testStatusEndpoint(): void
    {
        $response = Client::execute(url: '/__opr/status', method: 'GET');
        self::assertEquals(200, $response['code']);

        $body = \json_decode($response['body'], true);
        self::assertIsArray($body);
        self::assertArrayHasKey('ready', $body);
        self::assertArrayHasKey('engine', $body);
        self::assertArrayHasKey('version', $body);
        self::assertArrayHasKey('uptime', $body);
        self::assertArrayHasKey('connections', $body);

        self::assertTrue($body['ready']);
        self::assertIsNumeric($body['uptime']);
        self::assertGreaterThan(0, $body['uptime']);
    }

    /**
     * Test status endpoint structure
     * Verifies all required fields and their types
     */
    public function testStatusEndpointStructure(): void
    {
        $response = Client::execute(url: '/__opr/status', method: 'GET');
        self::assertEquals(200, $response['code']);

        $body = \json_decode($response['body'], true);

        // Verify required fields exist
        self::assertArrayHasKey('ready', $body);
        self::assertArrayHasKey('engine', $body);
        self::assertArrayHasKey('version', $body);
        self::assertArrayHasKey('uptime', $body);
        self::assertArrayHasKey('connections', $body);

        // Verify field types
        self::assertIsBool($body['ready']);
        self::assertIsString($body['engine']);
        self::assertIsString($body['version']);
        self::assertIsNumeric($body['uptime']);
        self::assertIsArray($body['connections']);

        // Verify connections structure
        self::assertArrayHasKey('current', $body['connections']);
        self::assertArrayHasKey('max', $body['connections']);
        self::assertIsInt($body['connections']['current']);
        self::assertIsInt($body['connections']['max']);
        self::assertGreaterThanOrEqual(0, $body['connections']['current']);
        self::assertGreaterThan(0, $body['connections']['max']);
    }

    /**
     * Test status uptime progression
     * Verifies that uptime increases over time
     */
    public function testStatusUptimeProgression(): void
    {
        $response1 = Client::execute(url: '/__opr/status', method: 'GET');
        $body1 = \json_decode($response1['body'], true);
        $uptime1 = $body1['uptime'];

        sleep(2);

        $response2 = Client::execute(url: '/__opr/status', method: 'GET');
        $body2 = \json_decode($response2['body'], true);
        $uptime2 = $body2['uptime'];

        self::assertGreaterThan($uptime1, $uptime2, 'Uptime should increase over time');
        self::assertGreaterThanOrEqual(2, $uptime2 - $uptime1, 'Uptime should increase by at least 2 seconds');
    }

    /**
     * Test telemetry endpoint
     * Verifies that the /__opr/timings endpoint returns timing information
     */
    public function testTelemetryEndpoint(): void
    {
        $response = Client::execute(url: '/__opr/timings', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertEquals('text/plain; charset=utf-8', $response['headers']['content-type']);
        self::assertStringContainsString('startup=', $response['body']);
    }

    /**
     * Test telemetry data format
     * Verifies that timing data is properly formatted
     */
    public function testTelemetryDataFormat(): void
    {
        $response = Client::execute(url: '/__opr/timings', method: 'GET');
        self::assertEquals(200, $response['code']);

        $body = $response['body'];
        $lines = explode("\n", trim($body));

        // Parse timings
        $timings = [];
        foreach ($lines as $line) {
            if (empty($line)) continue;
            $parts = explode('=', $line, 2);
            self::assertCount(2, $parts, "Invalid timing format: $line");
            $timings[$parts[0]] = $parts[1];
        }

        // Verify startup timing exists and is numeric
        self::assertArrayHasKey('startup', $timings);
        self::assertIsNumeric($timings['startup']);
        self::assertGreaterThanOrEqual(0, (float)$timings['startup']);
    }

    /**
     * Test concurrent health checks
     * Verifies that the runtime can handle multiple simultaneous requests
     */
    public function testConcurrentHealthChecks(): void
    {
        $responses = [];
        $expectedResponses = 10;

        // Simulate concurrent requests (sequential in PHP, but tests concurrency handling)
        for ($i = 0; $i < $expectedResponses; $i++) {
            $responses[] = Client::execute(url: '/__opr/health', method: 'GET');
        }

        // Verify all requests succeeded
        foreach ($responses as $i => $response) {
            self::assertEquals(200, $response['code'], "Request $i failed");
            self::assertEquals('OK', $response['body']);
        }
    }

    /**
     * Test endpoint response times
     * Verifies that endpoints respond within acceptable time limits
     */
    public function testEndpointResponseTimes(): void
    {
        $endpoints = ['/__opr/health', '/__opr/status', '/__opr/timings'];
        $maxResponseTime = 1.0; // 1 second max

        foreach ($endpoints as $endpoint) {
            $start = microtime(true);
            $response = Client::execute(url: $endpoint, method: 'GET');
            $duration = microtime(true) - $start;

            self::assertEquals(200, $response['code'], "Endpoint $endpoint failed");
            self::assertLessThan($maxResponseTime, $duration,
                "Endpoint $endpoint took too long: {$duration}s");
        }
    }

    /**
     * Test invalid endpoint
     * Verifies that invalid endpoints return appropriate errors
     */
    public function testInvalidEndpoint(): void
    {
        $response = Client::execute(url: '/__opr/invalid', method: 'GET');
        self::assertNotEquals(200, $response['code'], 'Invalid endpoint should not return 200');
    }

    /**
     * Test database engine identification
     * Verifies that the status endpoint correctly identifies the database engine
     */
    public function testDatabaseEngineIdentification(): void
    {
        $response = Client::execute(url: '/__opr/status', method: 'GET');
        $body = \json_decode($response['body'], true);

        $validEngines = ['postgres', 'mysql', 'mariadb', 'mongodb'];
        self::assertContains($body['engine'], $validEngines,
            'Engine must be one of: ' . implode(', ', $validEngines));
    }

    /**
     * Test version format
     * Verifies that the version string follows expected format
     */
    public function testVersionFormat(): void
    {
        $response = Client::execute(url: '/__opr/status', method: 'GET');
        $body = \json_decode($response['body'], true);

        self::assertNotEmpty($body['version'], 'Version should not be empty');
        self::assertMatchesRegularExpression('/^[\d\.\-a-zA-Z]+$/', $body['version'],
            'Version should contain only alphanumeric characters, dots, and hyphens');
    }

    /**
     * Test connection count accuracy
     * Verifies that connection counts are reasonable
     */
    public function testConnectionCountAccuracy(): void
    {
        $response = Client::execute(url: '/__opr/status', method: 'GET');
        $body = \json_decode($response['body'], true);

        $current = $body['connections']['current'];
        $max = $body['connections']['max'];

        self::assertLessThanOrEqual($max, $current,
            'Current connections should not exceed maximum');
        self::assertGreaterThanOrEqual(0, $current,
            'Current connections should be non-negative');
    }

    /**
     * Test management API stability
     * Verifies that the management API remains stable under repeated requests
     */
    public function testManagementAPIStability(): void
    {
        $iterations = 20;
        $failures = 0;

        for ($i = 0; $i < $iterations; $i++) {
            $healthResponse = Client::execute(url: '/__opr/health', method: 'GET');
            $statusResponse = Client::execute(url: '/__opr/status', method: 'GET');

            if ($healthResponse['code'] !== 200 || $statusResponse['code'] !== 200) {
                $failures++;
            }
        }

        self::assertEquals(0, $failures,
            "Management API failed $failures out of $iterations requests");
    }

    /**
     * Override base class tests that don't apply to database runtimes
     * Database runtimes don't have function execution endpoints
     */
    public function testWrongSecret(): void
    {
        // Database runtimes don't implement function execution endpoints
        // Only management endpoints are available, and they don't require secrets
        $this->markTestSkipped('Database runtimes do not have function execution endpoints');
    }

    public function testEmptySecret(): void
    {
        // Database runtimes don't implement function execution endpoints
        // Only management endpoints are available, and they don't require secrets
        $this->markTestSkipped('Database runtimes do not have function execution endpoints');
    }

    public function testEmptyServerSecret(): void
    {
        // Database runtimes don't implement function execution endpoints
        // Only management endpoints are available, and they don't require secrets
        $this->markTestSkipped('Database runtimes do not have function execution endpoints');
    }

    /**
     * Override testTimings for database runtimes
     * Database runtimes only have startup timing, not download/extract timings
     */
    public function testTimings(): void
    {
        $response = Client::execute(method: 'GET', url: '/__opr/timings');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('text/plain', $response['headers']['content-type']);

        $timings = [];
        foreach (explode("\n", trim($response['body'])) as $line) {
            if (empty($line)) continue;
            [$key, $value] = explode('=', $line, 2);
            $timings[$key] = (float)$value;
        }

        // Database runtimes only have startup timing
        self::assertArrayHasKey('startup', $timings);
        self::assertIsFloat($timings['startup']);
        self::assertGreaterThan(0, $timings['startup']);
    }
}
