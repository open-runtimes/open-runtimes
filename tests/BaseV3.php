<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

abstract class BaseV3 extends TestCase
{
    public function setUp(): void
    {
    }

    public function tearDown(): void
    {
    }

    private function execute($body = '', $url = '/', $method = 'POST', $headers = []) {
        $ch = \curl_init();

        $responseHeaders = [];
        $optArray = array(
            CURLOPT_URL => 'http://localhost:3000' . $url,
            CURLOPT_RETURNTRANSFER => true,
            CURLOPT_HEADERFUNCTION => function ($curl, $header) use (&$responseHeaders) {
                $len = strlen($header);
                $header = explode(':', $header, 2);
                if (count($header) < 2) // ignore invalid headers
                    return $len;
        
                $key = strtolower(trim($header[0]));
                $responseHeaders[$key] = trim($header[1]);

                if(\in_array($key, ['x-openruntimes-logs', 'x-openruntimes-errors'])) {
                    $responseHeaders[$key] = \urldecode($responseHeaders[$key]);
                }
        
                return $len;
            },
            CURLOPT_CUSTOMREQUEST => $method,
            CURLOPT_POSTFIELDS => \is_array($body) ? \json_encode($body, JSON_FORCE_OBJECT) : $body,
            CURLOPT_HEADEROPT => \CURLHEADER_UNIFIED,
            CURLOPT_HTTPHEADER => \array_merge(array('x-openruntimes-secret: ' . \getenv('INTERNAL_RUNTIME_KEY')), $headers)
        );
        
        \curl_setopt_array($ch, $optArray);

        $body = curl_exec($ch);
        $code = curl_getinfo($ch, \CURLINFO_HTTP_CODE);

        \curl_close($ch);

        return [
            'code' => $code,
            'body' => $body,
            'headers' => $responseHeaders
        ];
    }

    public function testPlaintextResponse(): void
    {
        $response = $this->execute(headers: ['x-action: plaintextResponse']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('Hello World 👋', $response['body']);
    }

    public function testJsonResponse(): void
    {
        $response = $this->execute(headers: ['x-action: jsonResponse']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('application/json', $response['headers']['content-type']);

        $body = \json_decode($response['body'], true);

        self::assertEquals(true, $body['json']);
        self::assertEquals('Developers are awesome.', $body['message']);
    }

    public function testFileResponse(): void
    {
        $response = $this->execute(headers: ['x-action: fileResponse']);

        self::assertEquals(200, $response['code']);
        self::assertEquals(2283072, \strlen($response['body']));
        self::assertEquals(2283072, $response['headers']['content-length']);
        self::assertEquals('image/png', $response['headers']['content-type']);
    }

    public function testRedirectResponse(): void
    {
        $response = $this->execute(headers: ['x-action: redirectResponse']);
        self::assertEquals(301, $response['code']);
        self::assertEmpty($response['body']);
        self::assertEquals('https://github.com/', $response['headers']['location']);
    }

    public function testEmptyResponse(): void
    {
        $response = $this->execute(headers: ['x-action: emptyResponse']);
        self::assertEquals(204, $response['code']);
        self::assertEmpty($response['body']);
        self::assertEquals('0', $response['headers']['content-length']);
    }

    public function testNoResponse(): void
    {
        $response = $this->execute(headers: ['x-action: noResponse']);
        self::assertEquals(204, $response['code']);
        self::assertEmpty($response['body']);
    }

    public function testDoubleResponse(): void
    {
        $response = $this->execute(headers: ['x-action: doubleResponse']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('This should be returned.', $response['body']);
    }

    public function testHeadersResponse(): void
    {
        $response = $this->execute(headers: ['x-action: headersResponse']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('OK', $response['body']);
        self::assertEquals('first-value', $response['headers']['first-header']);
        self::assertEquals('second-value', $response['headers']['second-header']);
    }

    public function testStatusResponse(): void
    {
        $response = $this->execute(headers: ['x-action: statusResponse']);
        self::assertEquals(404, $response['code']);
        self::assertEquals('FAIL', $response['body']);
    }

    public function testComplexResponse(): void
    {
        $response = $this->execute(headers: ['x-action: complexResponse']);
        self::assertEquals(201, $response['code']);
        self::assertEquals('👌', $response['body']);
        self::assertEquals('value1', $response['headers']['header1']);
    }

    public function testException(): void
    {
        $response = $this->execute(headers: ['x-action: nonExistingAction']);
        self::assertEquals(500, $response['code']);
        self::assertEmpty($response['body']);
        self::assertEmpty($response['headers']['x-openruntimes-logs']);
        self::assertStringContainsString('Unkonwn action', $response['headers']['x-openruntimes-errors']);
    }

    public function testWrongSecret(): void
    {
        $response = $this->execute(headers: ['x-openruntimes-secret: wrongSecret']);
        self::assertEquals(500, $response['code']);
        self::assertEquals('Unauthorized', $response['body']);
    }

    public function testRequestMethod(): void
    {
        $response = $this->execute(method: 'GET', headers: ['x-action: requestMethod']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('GET', $response['body']);

        $response = $this->execute(method: 'POST', headers: ['x-action: requestMethod']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('POST', $response['body']);

        $response = $this->execute(method: 'PUT', headers: ['x-action: requestMethod']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('PUT', $response['body']);

        $response = $this->execute(method: 'DELETE', headers: ['x-action: requestMethod']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('DELETE', $response['body']);
    }

    public function testRequestUrl(): void
    {
        $response = $this->execute(url: '/', headers: ['x-action: requestUrl']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('/', $response['body']);

        $response = $this->execute(url: '/some/path', headers: ['x-action: requestUrl']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('/some/path', $response['body']);

        $response = $this->execute(url: '/path?key=value', headers: ['x-action: requestUrl']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('/path?key=value', $response['body']);
    }

    public function testRequestHeaders(): void
    {
        $response = $this->execute(headers: ['x-action: requestHeaders', 'x-first-header: first-value', 'x-openruntimes-custom-header: should-be-hidden']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('application/json', $response['headers']['content-type']);

        $body = \json_decode($response['body'], true);

        self::assertEquals('requestHeaders', $body['x-action']);
        self::assertEquals('first-value', $body['x-first-header']);
        self::assertArrayNotHasKey('x-openruntimes-custom-header', $body);
    }

    public function testRequestBodyPlaintext(): void
    {
        $response = $this->execute(body: 'Hello 👋', headers: ['x-action: requestBodyPlaintext']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('Hello 👋', $response['body']);

        $response = $this->execute(body: '', headers: ['x-action: requestBodyPlaintext']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('', $response['body']);

        $response = $this->execute(headers: ['x-action: requestBodyPlaintext']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('', $response['body']);
    }

    public function testRequestBodyJson(): void
    {
        $response = $this->execute(body: '{"data":"OK"}', headers: ['x-action: requestBodyJson', 'content-type: application/json']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('OK', $response['body']);

        $response = $this->execute(body: '{"data":"OK"}', headers: ['x-action: requestBodyJson', 'content-type: text/plain']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('Missing key', $response['body']);
    }

    public function testEnvVars(): void
    {
        $response = $this->execute(headers: ['x-action: envVars']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('application/json', $response['headers']['content-type']);

        $body = \json_decode($response['body'], true);

        self::assertEquals('customValue', $body['var']);
        self::assertNull($body['emptyVar']);
    }

    public function testLogs(): void
    {
        $response = $this->execute(headers: ['x-action: logs' ]);
        self::assertEquals(200, $response['code']);
        self::assertEmpty($response['body']);
        self::assertStringContainsString('Debug log', $response['headers']['x-openruntimes-logs']);
        self::assertStringContainsString(42, $response['headers']['x-openruntimes-logs']);
        self::assertStringContainsString(4.2, $response['headers']['x-openruntimes-logs']);
        self::assertStringContainsString('true', \strtolower($response['headers']['x-openruntimes-logs'])); //strlower allows True in Python
        self::assertStringContainsString('Error log', $response['headers']['x-openruntimes-errors']);
    }
}