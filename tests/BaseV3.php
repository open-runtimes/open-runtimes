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

        $headers = \array_merge([
            'content-type' => 'text/plain',
            'x-open-runtimes-secret' => \getenv('INTERNAL_RUNTIME_KEY')
        ], $headers);
        $headersParsed = [];

        foreach ($headers as $header => $value) {
            $headersParsed[] = $header . ': ' . $value;
        }

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

                if(\in_array($key, ['x-open-runtimes-logs', 'x-open-runtimes-errors'])) {
                    $responseHeaders[$key] = \urldecode($responseHeaders[$key]);
                }
        
                return $len;
            },
            CURLOPT_CUSTOMREQUEST => $method,
            CURLOPT_POSTFIELDS => \is_array($body) ? \json_encode($body, JSON_FORCE_OBJECT) : $body,
            CURLOPT_HEADEROPT => \CURLHEADER_UNIFIED,
            CURLOPT_HTTPHEADER => $headersParsed
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
        $response = $this->execute(headers: ['x-action' => 'plaintextResponse']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('Hello World 👋', $response['body']);
    }

    public function testJsonResponse(): void
    {
        $response = $this->execute(headers: ['x-action' => 'jsonResponse']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('application/json', $response['headers']['content-type']);

        $body = \json_decode($response['body'], true);

        self::assertEquals(true, $body['json']);
        self::assertEquals('Developers are awesome.', $body['message']);
    }

    public function testFileResponse(): void
    {
        $response = $this->execute(headers: ['x-action' => 'fileResponse']);

        self::assertEquals(200, $response['code']);
        self::assertEquals(2283072, \strlen($response['body']));
        self::assertEquals(2283072, $response['headers']['content-length']);
        self::assertEquals('image/png', $response['headers']['content-type']);
    }


    public function testHtmlResponse(): void
    {
        $response = $this->execute(headers: ['x-action' => 'htmlResponse']);

        self::assertEquals(200, $response['code']);
        self::assertEquals('<h1>Title</h1>', $response['body']);
        self::assertEquals('text/html', $response['headers']['content-type']);
    }

    public function testRedirectResponse(): void
    {
        $response = $this->execute(headers: ['x-action' => 'redirectResponse']);
        self::assertEquals(301, $response['code']);
        self::assertEmpty($response['body']);
        self::assertEquals('https://github.com/', $response['headers']['location']);
    }

    public function testEmptyResponse(): void
    {
        $response = $this->execute(headers: ['x-action' => 'emptyResponse']);
        self::assertEquals(204, $response['code']);
        self::assertEmpty($response['body']);
        self::assertEquals('0', $response['headers']['content-length']);
    }

    public function testNoResponse(): void
    {
        $response = $this->execute(headers: ['x-action' => 'noResponse']);
        self::assertEquals(204, $response['code']);
        self::assertEmpty($response['body']);
    }

    public function testDoubleResponse(): void
    {
        $response = $this->execute(headers: ['x-action' => 'doubleResponse']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('This should be returned.', $response['body']);
    }

    public function testHeadersResponse(): void
    {
        $response = $this->execute(headers: ['x-action' => 'headersResponse']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('OK', $response['body']);
        self::assertEquals('first-value', $response['headers']['first-header']);
        self::assertEquals('second-value', $response['headers']['second-header']);
    }

    public function testStatusResponse(): void
    {
        $response = $this->execute(headers: ['x-action' => 'statusResponse']);
        self::assertEquals(404, $response['code']);
        self::assertEquals('FAIL', $response['body']);
    }

    public function testComplexResponse(): void
    {
        $response = $this->execute(headers: ['x-action' => 'complexResponse']);
        $body = \json_decode($response['body'], true);

        self::assertEquals(201, $response['code']);
        self::assertEquals(201, $body['code']);

        self::assertEquals('application/json', $body['contentType']);
        self::assertEquals('application/json', $response['headers']['content-type']);

        self::assertEquals('value1', $body['headers']['header1']);
        self::assertEquals('value1', $response['headers']['header1']);
        self::assertEquals('value1', $body['header1']);

        self::assertEquals('value2', $body['headers']['header2']);
        self::assertEquals('value2', $response['headers']['header2']);
        self::assertEquals('value2', $body['header2']);

        self::assertArrayHasKey('cookie1', $body['cookies']);
        self::assertArrayHasKey('cookie2', $body['cookies']);

        self::assertEquals('cookie1=value1; cookie2=value2; Domain=google.com; Expire=Wed, 21 Oct 2015 07:28:00 GMT; Max-Age=2592000; Path=/; SameSite=Lax; Secure; HttpOnly', $response['headers']['set-cookie']);

        self::assertEquals('👌', $body['body']);
    }

    public function testException(): void
    {
        $response = $this->execute(headers: ['x-action' => 'nonExistingAction']);
        self::assertEquals(500, $response['code']);
        self::assertEmpty($response['body']);
        self::assertEmpty($response['headers']['x-open-runtimes-logs']);
        self::assertStringContainsString('Unkonwn action', $response['headers']['x-open-runtimes-errors']);
    }

    public function testWrongSecret(): void
    {
        $response = $this->execute(headers: ['x-open-runtimes-secret' => 'wrongSecret']);
        self::assertEquals(500, $response['code']);
        self::assertEquals('Unauthorized', $response['body']);
    }

    public function testRequestMethod(): void
    {
        $response = $this->execute(method: 'GET', headers: ['x-action' => 'requestMethod']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('GET', $response['body']);

        $response = $this->execute(method: 'POST', headers: ['x-action' => 'requestMethod']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('POST', $response['body']);

        $response = $this->execute(method: 'PUT', headers: ['x-action' => 'requestMethod']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('PUT', $response['body']);

        $response = $this->execute(method: 'DELETE', headers: ['x-action' => 'requestMethod']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('DELETE', $response['body']);
    }

    public function testRequestUrl(): void
    {
        $response = $this->execute(url: '/', headers: ['x-action' => 'requestUrl']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('/', $response['body']);

        $response = $this->execute(url: '/some/path', headers: ['x-action' => 'requestUrl']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('/some/path', $response['body']);

        $response = $this->execute(url: '/path?key=value', headers: ['x-action' => 'requestUrl']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('/path?key=value', $response['body']);
    }

    public function testRequestHeaders(): void
    {
        $response = $this->execute(headers: ['x-action' => 'requestHeaders', 'x-first-header' => 'first-value', 'x-open-runtimes-custom-header' => 'should-be-hidden']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('application/json', $response['headers']['content-type']);

        $body = \json_decode($response['body'], true);

        self::assertEquals('requestHeaders', $body['x-action']);
        self::assertEquals('first-value', $body['x-first-header']);
        self::assertArrayNotHasKey('x-open-runtimes-custom-header', $body);
    }

    public function testRequestBodyPlaintext(): void
    {
        $response = $this->execute(body: 'Hello 👋', headers: ['x-action' => 'requestBodyPlaintext']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('Hello 👋', $response['body']);

        $response = $this->execute(body: '', headers: ['x-action' => 'requestBodyPlaintext']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('', $response['body']);

        $response = $this->execute(headers: ['x-action' => 'requestBodyPlaintext']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('', $response['body']);
    }

    public function testRequestBodyJson(): void
    {
        $response = $this->execute(body: '{"key1":"OK","key2":"👋"}', headers: ['x-action' => 'requestBodyJson', 'content-type' => 'application/json']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('{"key1":"OK","key2":"👋"}', $response['body']);

        $response = $this->execute(body: '{"data":"OK"}', headers: ['x-action' => 'requestBodyJson', 'content-type' => 'text/plain']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('{"key1":"Missing key","key2":"Missing key"}', $response['body']);
    }

    public function testRequestBodyUrlEncoded(): void
    {
        $response = $this->execute(body: 'key1=OK&key2=%F0%9F%91%8B', headers: ['x-action' => 'requestBodyUrlEncoded', 'content-type' => 'application/x-www-form-urlencoded']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('{"key1":"OK","key2":"👋"}', $response['body']);

        $response = $this->execute(body: 'Plain body', headers: ['x-action' => 'requestBodyUrlEncoded', 'content-type' => 'application/x-www-form-urlencoded']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('{"key1":"Missing key","key2":"Missing key"}', $response['body']);

        $response = $this->execute(body: 'key1=OK&key2=%F0%9F%91%8B', headers: ['x-action' => 'requestBodyUrlEncoded', 'content-type' => 'text/plain']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('{"key1":"Missing key","key2":"Missing key"}', $response['body']);
    }


    public function testRequestBodyFormData(): void
    {
        $formData = '--MY_AWESOME_BOUNDARY\nContent-Disposition: form-data; name="key1"\n\nOK\n--MY_AWESOME_BOUNDARY\nContent-Disposition: form-data; name="key2"\n\n👋\n--MY_AWESOME_BOUNDARY--"';

        $response = $this->execute(body: $formData, headers: ['x-action' => 'requestBodyFormData', 'content-type' => 'multipart/form-data; boundary=MY_AWESOME_BOUNDARY']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('{"key1":"OK","key2":"👋"}', $response['body']);

        $response = $this->execute(body: $formData, headers: ['x-action' => 'requestBodyFormData', 'content-type' => 'multipart/form-data']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('{"key1":"Missing key","key2":"Missing key"}', $response['body']);

        $response = $this->execute(body: $formData, headers: ['x-action' => 'requestBodyFormData', 'content-type' => 'text/plain']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('{"key1":"Missing key","key2":"Missing key"}', $response['body']);
    }

    public function testEnvVars(): void
    {
        $response = $this->execute(headers: ['x-action' => 'envVars']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('application/json', $response['headers']['content-type']);

        $body = \json_decode($response['body'], true);

        self::assertEquals('customValue', $body['var']);
        self::assertNull($body['emptyVar']);
    }

    public function testLogs(): void
    {
        $response = $this->execute(headers: ['x-action' => 'logs' ]);
        self::assertEquals(200, $response['code']);
        self::assertEmpty($response['body']);
        self::assertStringContainsString('Debug log', $response['headers']['x-open-runtimes-logs']);
        self::assertStringContainsString(42, $response['headers']['x-open-runtimes-logs']);
        self::assertStringContainsString(4.2, $response['headers']['x-open-runtimes-logs']);
        self::assertStringContainsString('true', \strtolower($response['headers']['x-open-runtimes-logs'])); //strlower allows True in Python
        self::assertStringContainsString('Error log', $response['headers']['x-open-runtimes-errors']);
    }
}