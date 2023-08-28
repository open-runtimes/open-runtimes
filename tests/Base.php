<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class Base extends TestCase
{
    public function setUp(): void
    {
    }

    public function tearDown(): void
    {
    }

    private function execute($body = '', $url = '/', $method = 'POST', $headers = [], $port = 3000) {
        $ch = \curl_init();

        $headers = \array_merge([
            'content-type' => 'text/plain',
            'x-open-runtimes-secret' => \getenv('OPEN_RUNTIMES_SECRET')
        ], $headers);
        $headersParsed = [];

        foreach ($headers as $header => $value) {
            $headersParsed[] = $header . ': ' . $value;
        }

        $responseHeaders = [];
        $optArray = [
            CURLOPT_URL => 'http://localhost:' . $port . $url,
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
            CURLOPT_HTTPHEADER => $headersParsed,
            CURLOPT_TIMEOUT => 5
        ];
        
        \curl_setopt_array($ch, $optArray);

        $body = curl_exec($ch);
        $code = curl_getinfo($ch, \CURLINFO_HTTP_CODE);

        if (curl_errno($ch)) {
            \var_dump(curl_error($ch));
        }

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
        self::assertEqualsIgnoringWhitespace('text/plain; charset=utf-8', $response['headers']['content-type']);
    }

    public function testJsonResponse(): void
    {
        $response = $this->execute(headers: ['x-action' => 'jsonResponse']);
        self::assertEquals(200, $response['code']);
        self::assertEqualsIgnoringWhitespace('application/json; charset=utf-8', $response['headers']['content-type']);

        $body = \json_decode($response['body'], true);

        self::assertEquals(true, $body['json']);
        self::assertEquals('Developers are awesome.', $body['message']);
    }

    public function testContentTypeResponse(): void 
    {
        $response = $this->execute(headers: ['x-action' => 'customCharsetResponse']);
        self::assertEquals(200, $response['code']);
        self::assertEqualsIgnoringWhitespace('text/plain; charset=iso-8859-1', $response['headers']['content-type']);

        $response = $this->execute(headers: ['x-action' => 'multipartResponse']);
        self::assertEquals(200, $response['code']);
        self::assertEqualsIgnoringWhitespace('multipart/form-data; boundary=12345', $response['headers']['content-type']);
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
    }

    public function testNoResponse(): void
    {
        $response = $this->execute(headers: ['x-action' => 'noResponse']);
        self::assertEquals(500, $response['code']);
        self::assertEmpty($response['body']);
        self::assertStringContainsString('Return statement missing.', $response['headers']['x-open-runtimes-errors']);
    }

    public function testDoubleResponse(): void
    {
        $response = $this->execute(headers: ['x-action' => 'doubleResponse']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('This should be returned.', $response['body']);
    }

    public function testHeadersResponse(): void
    {
        $response = $this->execute(headers: ['x-action' => 'headersResponse', 'x-open-runtimes-custom-in-header' => 'notMissing']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('OK', $response['body']);
        self::assertEquals('first-value', $response['headers']['first-header']);
        self::assertEquals('missing', $response['headers']['second-header']);
        self::assertArrayNotHasKey('x-open-runtimes-custom-out-header', $response['headers']);
    }

    public function testStatusResponse(): void
    {
        $response = $this->execute(headers: ['x-action' => 'statusResponse']);
        self::assertEquals(404, $response['code']);
        self::assertEquals('FAIL', $response['body']);
    }

    public function testException(): void
    {
        $response = $this->execute(headers: ['x-action' => 'nonExistingAction']);
        self::assertEquals(500, $response['code']);
        self::assertEmpty($response['body']);
        self::assertEmpty($response['headers']['x-open-runtimes-logs']);
        self::assertStringContainsString('Unknown action', $response['headers']['x-open-runtimes-errors']);

        $entrypoint = \getenv('OPEN_RUNTIMES_ENTRYPOINT');

        // Fix for dart (expected behaviour)
        if(\str_starts_with($entrypoint, 'lib/')) {
            $entrypoint = implode('', explode('lib', $entrypoint, 2));
        }

        self::assertStringContainsString($entrypoint, $response['headers']['x-open-runtimes-errors']);
    }

    public function testWrongSecret(): void
    {
        $response = $this->execute(headers: ['x-open-runtimes-secret' => 'wrongSecret']);
        self::assertEquals(500, $response['code']);
        self::assertEquals('Unauthorized. Provide correct "x-open-runtimes-secret" header.', $response['body']);
    }


    public function testEmptySecret(): void
    {
        $response = $this->execute(headers: ['x-action' => 'plaintextResponse', 'x-open-runtimes-secret' => '']);
        self::assertEquals(500, $response['code']);
        self::assertEquals('Unauthorized. Provide correct "x-open-runtimes-secret" header.', $response['body']);
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

        $response = $this->execute(method: 'OPTIONS', headers: ['x-action' => 'requestMethod']);
        self::assertEquals(200, $response['code']);
        // Bug in C++ framework makes this an empty string
        // self::assertEquals('OPTIONS', $response['body']);

        $response = $this->execute(method: 'PATCH', headers: ['x-action' => 'requestMethod']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('PATCH', $response['body']);
    }

    public function testRequestUrl(): void
    {
        $response = $this->execute(url: '/', headers: ['x-action' => 'requestUrl']);
        self::assertEquals(200, $response['code']);

        $body = \json_decode($response['body'], true);

        self::assertEquals(3000, $body['port']);
        self::assertEquals('/', $body['path']);
        self::assertIsArray($body['query']);
        self::assertEmpty($body['query']);
        self::assertEquals('', $body['queryString']);
        self::assertEquals('http', $body['scheme']);
        self::assertContains($body['host'], ['localhost', '0.0.0.0', '127.0.0.1']);
        self::assertContains($body['url'], ['http://localhost:3000/', 'http://0.0.0.0:3000/', 'http://127.0.0.1:3000/']);

        $response = $this->execute(url: '/a/b?c=d&e=f#something', headers: [
            'x-action' => 'requestUrl',
            'x-forwarded-proto' => 'https',
            'host' => 'www.mydomain.com:3001'
        ]);
        self::assertEquals(200, $response['code']);

        $body = \json_decode($response['body'], true);

        self::assertEquals(3001, $body['port']);
        self::assertEquals('/a/b', $body['path']);
        self::assertIsArray($body['query']);
        self::assertCount(2, $body['query']);
        self::assertEquals('d', $body['query']['c']);
        self::assertEquals('f', $body['query']['e']);
        self::assertEquals('c=d&e=f', $body['queryString']);
        self::assertEquals('https', $body['scheme']);
        self::assertEquals('www.mydomain.com', $body['host']);
        self::assertEquals('https://www.mydomain.com:3001/a/b?c=d&e=f', $body['url']);

        $response = $this->execute(url: '/', headers: ['x-action' => 'requestUrl', 'host' => 'www.mydomain.com:80']);
        self::assertEquals(200, $response['code']);
        $body = \json_decode($response['body'], true);
        self::assertEquals(80, $body['port']);
        self::assertEquals('www.mydomain.com', $body['host']);
        self::assertEquals('http://www.mydomain.com/', $body['url']);

        $response = $this->execute(url: '/?a=b&c==d&e=f=g=&h&i=j&&k', headers: ['x-action' => 'requestUrl']);
        self::assertEquals(200, $response['code']);
        $body = \json_decode($response['body'], true);
        self::assertEquals('b', $body['query']['a']);
        self::assertEquals('=d', $body['query']['c']);
        self::assertEquals('', $body['query']['h']);
        self::assertEquals('j', $body['query']['i']);
        self::assertEquals('', $body['query']['k']);
        self::assertArrayNotHasKey('l', $body['query']);

        $response = $this->execute(url: '/', headers: ['x-action' => 'requestUrl', 'host' => 'myapp.com', 'x-forwarded-proto' => 'https']);
        self::assertEquals(200, $response['code']);
        $body = \json_decode($response['body'], true);
        self::assertEquals('myapp.com', $body['host']);
        self::assertEquals(443, $body['port']);
        self::assertEquals('https://myapp.com/', $body['url']);

        $response = $this->execute(url: '/', headers: ['x-action' => 'requestUrl', 'host' => 'myapp.com:443', 'x-forwarded-proto' => 'https']);
        self::assertEquals(200, $response['code']);
        $body = \json_decode($response['body'], true);
        self::assertEquals('myapp.com', $body['host']);
        self::assertEquals(443, $body['port']);
        self::assertEquals('https://myapp.com/', $body['url']);

        $response = $this->execute(url: '/', headers: ['x-action' => 'requestUrl', 'host' => 'myapp.com:80', 'x-forwarded-proto' => 'https']);
        self::assertEquals(200, $response['code']);
        $body = \json_decode($response['body'], true);
        self::assertEquals('myapp.com', $body['host']);
        self::assertEquals(80, $body['port']);
        self::assertEquals('https://myapp.com:80/', $body['url']);

        $response = $this->execute(url: '/', headers: ['x-action' => 'requestUrl', 'host' => 'myapp.com:80', 'x-forwarded-proto' => 'http']);
        self::assertEquals(200, $response['code']);
        $body = \json_decode($response['body'], true);
        self::assertEquals('myapp.com', $body['host']);
        self::assertEquals(80, $body['port']);
        self::assertEquals('http://myapp.com/', $body['url']);
    }

    public function testRequestHeaders(): void
    {
        $response = $this->execute(headers: ['x-action' => 'requestHeaders', 'x-first-header' => 'first-value', 'x-open-runtimes-custom-header' => 'should-be-hidden']);
        self::assertEquals(200, $response['code']);
        self::assertEqualsIgnoringWhitespace('application/json; charset=utf-8', $response['headers']['content-type']);

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
        $response = $this->execute(body: '{"key1":"OK","key2":"👋","key3":"value3"}', headers: ['x-action' => 'requestBodyJson', 'content-type' => 'application/json']);
        self::assertEquals(200, $response['code']);

        $body = \json_decode($response['body'], true);

        self::assertEquals('OK', $body['key1']);
        self::assertEquals('👋', $body['key2']);
        self::assertEquals('{"key1":"OK","key2":"👋","key3":"value3"}', $body['raw']);

        $response = $this->execute(body: '{"data":"OK"}', headers: ['x-action' => 'requestBodyJson', 'content-type' => 'text/plain']);
        self::assertEquals(200, $response['code']);

        $body = \json_decode($response['body'], true);

        self::assertEquals('Missing key', $body['key1']);
        self::assertEquals('Missing key', $body['key2']);
        self::assertEquals('{"data":"OK"}', $body['raw']);
    }

    public function testEnvVars(): void
    {
        $response = $this->execute(headers: ['x-action' => 'envVars']);
        self::assertEquals(200, $response['code']);
        self::assertEqualsIgnoringWhitespace('application/json; charset=utf-8', $response['headers']['content-type']);

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
        self::assertStringContainsString('true', \strtolower($response['headers']['x-open-runtimes-logs'])); // strlower allows True in Python
        self::assertStringContainsString('Error log', $response['headers']['x-open-runtimes-errors']);
        self::assertStringNotContainsString('Native log', $response['headers']['x-open-runtimes-logs']);
        self::assertStringContainsString('Unsupported log detected.', $response['headers']['x-open-runtimes-logs']);
        self::assertStringContainsString('{"objectKey":"objectValue"}', $response['headers']['x-open-runtimes-logs']);
        self::assertStringContainsString('["arrayValue"]', $response['headers']['x-open-runtimes-logs']);
    }

    public function testLibrary(): void
    {
        $response = $this->execute(headers: ['x-action' => 'library'], body: '5');
        self::assertEquals(200, $response['code']);

        $body = \json_decode($response['body'], true);

        self::assertEquals('1', $body['todo']['userId']);
        self::assertEquals('5', $body['todo']['id']);
        self::assertEquals('laboriosam mollitia et enim quasi adipisci quia provident illum', $body['todo']['title']);
        self::assertEquals(false, $body['todo']['completed']);
    }

    public function testTimeout(): void
    {
        $response = $this->execute(headers: ['x-action' => 'timeout', 'x-open-runtimes-timeout' => '1']);
        self::assertEquals(500, $response['code']);
        self::assertEquals('', $response['body']);
        self::assertStringContainsString('Execution timed out.', $response['headers']['x-open-runtimes-errors']);
        self::assertStringContainsString('Timeout start.', $response['headers']['x-open-runtimes-logs']);
        self::assertStringNotContainsString('Timeout end.', $response['headers']['x-open-runtimes-logs']);

        $response = $this->execute(headers: ['x-action' => 'timeout', 'x-open-runtimes-timeout' => '5']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('Successful response.', $response['body']);
        self::assertStringContainsString('Timeout start.', $response['headers']['x-open-runtimes-logs']);
        self::assertStringContainsString('Timeout end.', $response['headers']['x-open-runtimes-logs']);

        $response = $this->execute(headers: ['x-action' => 'timeout', 'x-open-runtimes-timeout' => 'abcd']);
        self::assertEquals(500, $response['code']);
        self::assertEquals('Header "x-open-runtimes-timeout" must be an integer greater than 0.', $response['body']);
    }

    function assertEqualsIgnoringWhitespace($expected, $actual, $message = '') {
        $expected = preg_replace('/\s+/', '', $expected);
        $actual = preg_replace('/\s+/', '', $actual);
        self::assertEquals($expected, $actual, $message);
    }
}