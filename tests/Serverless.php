<?php

namespace Tests;

class Serverless extends Base
{
    public function testPlaintextResponse(): void
    {
        $response = Client::execute(headers: ['x-action' => 'plaintextResponse']);
        self::assertSame(200, $response['code']);
        self::assertSame('Hello World 👋', $response['body']);
        self::assertEqualsIgnoringWhitespace('text/plain; charset=utf-8', $response['headers']['content-type']);
    }

    public function testJsonResponse(): void
    {
        $response = Client::execute(headers: ['x-action' => 'jsonResponse']);
        self::assertSame(200, $response['code']);
        self::assertEqualsIgnoringWhitespace('application/json; charset=utf-8', $response['headers']['content-type']);

        $body = \json_decode($response['body'], true);

        self::assertSame(true, $body['json']);
        self::assertSame('Developers are awesome.', $body['message']);

        $body = \json_encode([ "name" => "OpenRntimes", "version" => 3.5, "published" => true, "nested" => [ [ 'object' => 1 ] ] ]);
        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyJson']);
        self::assertSame(200, $response['code']);
        self::assertStringNotContainsStringIgnoringCase(" ", $response['body']);
        self::assertStringNotContainsStringIgnoringCase("\n", $response['body']);
    }

    public function testContentTypeResponse(): void
    {
        $response = Client::execute(headers: ['x-action' => 'customCharsetResponse']);
        self::assertSame(200, $response['code']);
        self::assertEqualsIgnoringWhitespace('text/plain; charset=iso-8859-1', $response['headers']['content-type']);

        $response = Client::execute(headers: ['x-action' => 'uppercaseCharsetResponse']);
        self::assertSame(200, $response['code']);
        self::assertEqualsIgnoringWhitespace('text/plain; charset=utf-8', $response['headers']['content-type']);

        $response = Client::execute(headers: ['x-action' => 'multipartResponse']);
        self::assertSame(200, $response['code']);
        self::assertEqualsIgnoringWhitespace('multipart/form-data; boundary=12345', $response['headers']['content-type']);
    }

    public function testRedirectResponse(): void
    {
        $response = Client::execute(headers: ['x-action' => 'redirectResponse']);
        self::assertSame(301, $response['code']);
        self::assertEmpty($response['body']);
        self::assertSame('https://github.com/', $response['headers']['location']);
    }

    public function testEmptyResponse(): void
    {
        $response = Client::execute(headers: ['x-action' => 'emptyResponse']);
        self::assertSame(204, $response['code']);
        self::assertEmpty($response['body']);
    }

    public function testNoResponse(): void
    {
        $response = Client::execute(headers: ['x-action' => 'noResponse']);
        self::assertSame(500, $response['code']);
        self::assertEmpty($response['body']);
        self::assertStringContainsString('Return statement missing.', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }

    public function testDoubleResponse(): void
    {
        $response = Client::execute(headers: ['x-action' => 'doubleResponse']);
        self::assertSame(200, $response['code']);
        self::assertSame('This should be returned.', $response['body']);
    }

    public function testHeadersResponse(): void
    {
        $response = Client::execute(headers: ['x-action' => 'headersResponse', 'x-open-runtimes-custom-in-header' => 'notMissing', 'cookie' => 'cookieName=cookieValue; cookie2=value2; cookie3=value=3; cookie4=val:ue4; cookie5=value5']);
        self::assertSame(200, $response['code']);
        self::assertSame('OK', $response['body']);
        self::assertSame('first-value', $response['headers']['first-header']);
        self::assertSame('missing', $response['headers']['second-header']);

        $cookies = \explode('; ', $response['headers']['cookie']);
        self::assertCount(5, $cookies);
        self::assertContains("cookieName=cookieValue", $cookies);
        self::assertContains("cookie2=value2", $cookies);
        self::assertContains("cookie3=value=3", $cookies);
        self::assertContains("cookie4=val:ue4", $cookies);
        self::assertContains("cookie5=value5", $cookies);

        self::assertArrayNotHasKey('x-open-runtimes-custom-out-header', $response['headers']);
    }

    public function testStatusResponse(): void
    {
        $response = Client::execute(headers: ['x-action' => 'statusResponse']);
        self::assertSame(404, $response['code']);
        self::assertSame('FAIL', $response['body']);
    }

    public function testException(): void
    {
        $response = Client::execute(headers: ['x-action' => 'nonExistingAction']);
        self::assertSame(500, $response['code']);
        self::assertEmpty($response['body']);
        self::assertEmpty(Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('Unknown action', Client::getErrors($response['headers']['x-open-runtimes-log-id']));

        $entrypoint = \getenv('OPEN_RUNTIMES_ENTRYPOINT');

        // Fix for Dart - only has file name
        if(\str_starts_with($entrypoint, 'lib/')) {
            $entrypoint = implode('', explode('lib', $entrypoint, 2));
        }

        self::assertStringContainsString($entrypoint, Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }

    public function testErrorHandling(): void
    {
        $response = Client::execute(headers: ['x-action' => 'errorTest']);
        $logId = $response['headers']['x-open-runtimes-log-id'];
        $logs = Client::getLogs($logId);
        $errors = Client::getErrors($logId);
        
        self::assertSame(500, $response['code']);
        self::assertEmpty($response['body']);
        self::assertStringContainsString('Before error...', $logs);
        self::assertStringContainsString('Error!', $errors);
    }

    public function testRequestMethod(): void
    {
        $response = Client::execute(method: 'GET', headers: ['x-action' => 'requestMethod']);
        self::assertSame(200, $response['code']);
        self::assertSame('GET', $response['body']);

        $response = Client::execute(method: 'POST', headers: ['x-action' => 'requestMethod']);
        self::assertSame(200, $response['code']);
        self::assertSame('POST', $response['body']);

        $response = Client::execute(method: 'PUT', headers: ['x-action' => 'requestMethod']);
        self::assertSame(200, $response['code']);
        self::assertSame('PUT', $response['body']);

        $response = Client::execute(method: 'DELETE', headers: ['x-action' => 'requestMethod']);
        self::assertSame(200, $response['code']);
        self::assertSame('DELETE', $response['body']);

        $response = Client::execute(method: 'OPTIONS', headers: ['x-action' => 'requestMethod']);
        self::assertSame(200, $response['code']);
        // Bug in C++ framework makes this an empty string
        // self::assertSame('OPTIONS', $response['body']);

        $response = Client::execute(method: 'PATCH', headers: ['x-action' => 'requestMethod']);
        self::assertSame(200, $response['code']);
        self::assertSame('PATCH', $response['body']);
    }

    public function testRequestUrl(): void
    {
        $response = Client::execute(url: '/', headers: ['x-action' => 'requestUrl']);
        self::assertSame(200, $response['code']);

        $body = \json_decode($response['body'], true);

        self::assertSame(3000, $body['port']);
        self::assertSame('/', $body['path']);
        self::assertIsArray($body['query']);
        self::assertEmpty($body['query']);
        self::assertSame('', $body['queryString']);
        self::assertSame('http', $body['scheme']);
        self::assertContains($body['host'], ['localhost', '0.0.0.0', '127.0.0.1', 'open-runtimes-test-serve-secondary', 'open-runtimes-test-serve']);
        self::assertContains($body['url'], ['http://localhost:3000/', 'http://0.0.0.0:3000/', 'http://127.0.0.1:3000/', 'http://open-runtimes-test-serve-secondary:3000/', 'http://open-runtimes-test-serve:3000/']);

        $response = Client::execute(url: '/a/b?c=d&e=f#something', headers: [
            'x-action' => 'requestUrl',
            'x-forwarded-proto' => 'https',
            'host' => 'www.mydomain.com:3001'
        ]);
        self::assertSame(200, $response['code']);

        $body = \json_decode($response['body'], true);

        self::assertSame(3001, $body['port']);
        self::assertSame('/a/b', $body['path']);
        self::assertIsArray($body['query']);
        self::assertCount(2, $body['query']);
        self::assertSame('d', $body['query']['c']);
        self::assertSame('f', $body['query']['e']);
        self::assertSame('c=d&e=f', $body['queryString']);
        self::assertSame('https', $body['scheme']);
        self::assertSame('www.mydomain.com', $body['host']);
        self::assertSame('https://www.mydomain.com:3001/a/b?c=d&e=f', $body['url']);

        $response = Client::execute(url: '/', headers: ['x-action' => 'requestUrl', 'host' => 'www.mydomain.com:80']);
        self::assertSame(200, $response['code']);
        $body = \json_decode($response['body'], true);
        self::assertSame(80, $body['port']);
        self::assertSame('www.mydomain.com', $body['host']);
        self::assertSame('http://www.mydomain.com/', $body['url']);

        $response = Client::execute(url: '/?a=b&c==d&e=f=g=&h&i=j&&k', headers: ['x-action' => 'requestUrl']);
        self::assertSame(200, $response['code']);
        $body = \json_decode($response['body'], true);
        self::assertSame('b', $body['query']['a']);
        self::assertSame('=d', $body['query']['c']);
        self::assertSame('', $body['query']['h']);
        self::assertSame('j', $body['query']['i']);
        self::assertSame('', $body['query']['k']);
        self::assertArrayNotHasKey('l', $body['query']);

        $response = Client::execute(url: '/', headers: ['x-action' => 'requestUrl', 'host' => 'myapp.com', 'x-forwarded-proto' => 'https']);
        self::assertSame(200, $response['code']);
        $body = \json_decode($response['body'], true);
        self::assertSame('myapp.com', $body['host']);
        self::assertSame(443, $body['port']);
        self::assertSame('https://myapp.com/', $body['url']);

        $response = Client::execute(url: '/', headers: ['x-action' => 'requestUrl', 'host' => 'myapp.com:443', 'x-forwarded-proto' => 'https']);
        self::assertSame(200, $response['code']);
        $body = \json_decode($response['body'], true);
        self::assertSame('myapp.com', $body['host']);
        self::assertSame(443, $body['port']);
        self::assertSame('https://myapp.com/', $body['url']);

        $response = Client::execute(url: '/', headers: ['x-action' => 'requestUrl', 'host' => 'myapp.com:80', 'x-forwarded-proto' => 'https']);
        self::assertSame(200, $response['code']);
        $body = \json_decode($response['body'], true);
        self::assertSame('myapp.com', $body['host']);
        self::assertSame(80, $body['port']);
        self::assertSame('https://myapp.com:80/', $body['url']);

        $response = Client::execute(url: '/', headers: ['x-action' => 'requestUrl', 'host' => 'myapp.com:80', 'x-forwarded-proto' => 'http']);
        self::assertSame(200, $response['code']);
        $body = \json_decode($response['body'], true);
        self::assertSame('myapp.com', $body['host']);
        self::assertSame(80, $body['port']);
        self::assertSame('http://myapp.com/', $body['url']);
    }

    public function testRequestHeaders(): void
    {
        $response = Client::execute(headers: ['x-action' => 'requestHeaders', 'x-first-header' => 'first-value', 'x-open-runtimes-custom-header' => 'should-be-hidden']);
        self::assertSame(200, $response['code']);
        self::assertEqualsIgnoringWhitespace('application/json; charset=utf-8', $response['headers']['content-type']);

        $body = \json_decode($response['body'], true);

        self::assertSame('requestHeaders', $body['x-action']);
        self::assertSame('first-value', $body['x-first-header']);
        self::assertArrayNotHasKey('x-open-runtimes-custom-header', $body);


        $response = Client::execute(headers: ['x-action' => 'requestHeaders', 'X-UpPeRcAsE-KeY' => 'value']);
        self::assertSame(200, $response['code']);
        self::assertEqualsIgnoringWhitespace('application/json; charset=utf-8', $response['headers']['content-type']);

        $body = \json_decode($response['body'], true);

        self::assertSame('requestHeaders', $body['x-action']);
        self::assertSame('value', $body['x-uppercase-key']);
        self::assertArrayNotHasKey('X-UpPeRcAsE-KeY', $body);
    }

    public function testRequestBodyText(): void
    {
        $body = 'Hello 👋';
        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyText']);
        self::assertSame(200, $response['code']);
        self::assertSame($body, $response['body']);

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyTextAuto']);
        self::assertSame(200, $response['code']);
        self::assertSame($body, $response['body']);

        $response = Client::execute(body: '', headers: ['x-action' => 'requestBodyText']);
        self::assertSame(200, $response['code']);
        self::assertSame('', $response['body']);

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyTextAuto', 'content-type' => 'application/xhtml+xml']);
        self::assertSame(200, $response['code']);
        self::assertSame($body, $response['body']);
        self::assertTrue(\mb_check_encoding($response['body'], 'UTF-8'));

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyTextAuto', 'content-type' => 'not-application/json']);
        self::assertSame(200, $response['code']);
        self::assertSame($body, $response['body']);
        self::assertTrue(\mb_check_encoding($response['body'], 'UTF-8'));

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyTextAuto', 'content-type' => 'not-video/mp4']);
        self::assertSame(200, $response['code']);
        self::assertSame($body, $response['body']);
        self::assertTrue(\mb_check_encoding($response['body'], 'UTF-8'));

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyTextAuto', 'content-type' => 'application/octet-stream']);
        self::assertSame(200, $response['code']);
        self::assertSame($body, $response['body']);
        self::assertTrue(\mb_check_encoding($response['body'], 'UTF-8'));

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyTextAuto', 'content-type' => 'audio/mpeg']);
        self::assertSame(200, $response['code']);
        self::assertSame($body, $response['body']);
        self::assertTrue(\mb_check_encoding($response['body'], 'UTF-8'));

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyTextAuto', 'content-type' => 'font/ttf']);
        self::assertSame(200, $response['code']);
        self::assertSame($body, $response['body']);
        self::assertTrue(\mb_check_encoding($response['body'], 'UTF-8'));

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyTextAuto', 'content-type' => 'image/png']);
        self::assertSame(200, $response['code']);
        self::assertSame($body, $response['body']);
        self::assertTrue(\mb_check_encoding($response['body'], 'UTF-8'));

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyTextAuto', 'content-type' => 'video/mp4']);
        self::assertSame(200, $response['code']);
        self::assertSame($body, $response['body']);
        self::assertTrue(\mb_check_encoding($response['body'], 'UTF-8'));
    }

    public function testRequestBodyJson(): void
    {
        $body = '{"key1":"OK 👋","key2":true,"key3":3}';

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyJson', 'content-type' => 'application/json']);
        self::assertSame(200, $response['code']);

        $body = \json_decode($response['body'], true);
        self::assertSame('OK 👋', $body['key1']);
        self::assertSame(true, $body['key2']);
        self::assertSame(3, $body['key3']);

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyJsonAuto', 'content-type' => 'application/json']);
        self::assertSame(200, $response['code']);

        $body = \json_decode($response['body'], true);
        self::assertSame('OK 👋', $body['key1']);
        self::assertSame(true, $body['key2']);
        self::assertSame(3, $body['key3']);

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyJsonAuto', 'content-type' => 'ApPlIcAtIoN/JSON']);
        self::assertSame(200, $response['code']);

        $body = \json_decode($response['body'], true);
        self::assertSame('OK 👋', $body['key1']);
        self::assertSame(true, $body['key2']);
        self::assertSame(3, $body['key3']);

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyJson', 'content-type' => 'text/plain']);
        self::assertSame(200, $response['code']);

        $body = \json_decode($response['body'], true);
        self::assertSame('OK 👋', $body['key1']);
        self::assertSame(true, $body['key2']);
        self::assertSame(3, $body['key3']);

        $response = Client::execute(body: '', headers: ['x-action' => 'requestBodyJsonAuto', 'content-type' => 'application/json']);
        self::assertSame(200, $response['code']);
        self::assertSame('{}', $response['body']);
    }

    public function testRequestBodyBinary(): void
    {
        $body = \hex2bin("0123456789abcdef");

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyBinary', 'content-type' => 'application/octet-stream']);
        self::assertSame(200, $response['code']);
        self::assertSame($body, $response['body']);

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyBinary', 'content-type' => 'text/plain']);
        self::assertSame(200, $response['code']);
        self::assertSame($body, $response['body']);

        $body = pack('C*', ...[0,10,255]);

        $response = Client::execute(body: $body, headers: ['x-action' => 'requestBodyBinary', 'content-type' => 'text/plain']);
        self::assertSame(200, $response['code']);
        $bytes = \unpack('C*byte', $response['body']);
        self::assertCount(3, $bytes);
        self::assertSame(0, $bytes['byte1']);
        self::assertSame(10, $bytes['byte2']);
        self::assertSame(255, $bytes['byte3']);
    }

    public function testEnvVars(): void
    {
        $response = Client::execute(headers: ['x-action' => 'envVars']);
        self::assertSame(200, $response['code']);
        self::assertEqualsIgnoringWhitespace('application/json; charset=utf-8', $response['headers']['content-type']);

        $body = \json_decode($response['body'], true);

        self::assertSame('customValue', $body['var']);
        self::assertNull($body['emptyVar']);
    }

    public function testLogs(): void
    {
        $response = Client::execute(headers: ['x-action' => 'logs' ]);
        $logId = $response['headers']['x-open-runtimes-log-id'];
        $logs = Client::getLogs($logId);
        $errors = Client::getErrors($logId);
        self::assertSame(20, \strlen($logId));
        self::assertSame(200, $response['code']);
        self::assertEmpty($response['body']);
        self::assertStringContainsString('Debug log', $logs);
        self::assertStringContainsString(42, $logs);
        self::assertStringContainsString(4.2, $logs);
        self::assertStringContainsString('true', \strtolower($logs)); // strlower allows True in Python
        self::assertStringContainsString('Error log', $errors);
        self::assertStringContainsString('Native log', $logs);
        self::assertStringContainsString('Native logs detected.', $logs);
        self::assertStringContainsString('objectKey', $logs);
        self::assertStringContainsString('objectValue', $logs);
        self::assertStringContainsString('arrayValue', $logs);
        self::assertStringContainsString('Log+With+Plus+Symbol', $logs);
        self::assertStringContainsString("\n", $logs);
        self::assertStringContainsString('... Log truncated due to size limit (8000 characters)', $logs);
        self::assertStringContainsString('... Log truncated due to size limit (8000 characters)', $errors);
        self::assertGreaterThanOrEqual(9, \count(\explode("\n", $logs))); // Ensures each logs is on new line
        self::assertGreaterThanOrEqual(1, \count(\explode("\n", $errors))); // Ensures each error is on new line

        $response = Client::execute(headers: ['x-action' => 'logs' ]);
        $logIdSecond = $response['headers']['x-open-runtimes-log-id'];
        self::assertSame(20, \strlen($logId));
        self::assertNotEquals($logId, $logIdSecond);

        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-logging' => 'disabled', 'x-open-runtimes-log-id' => 'noLogs' ]);
        $logs = Client::getLogs('noLogs');
        $errors = Client::getErrors('noLogs');
        self::assertEmpty($response['headers']['x-open-runtimes-log-id']);
        self::assertEmpty($logs);
        self::assertEmpty($errors);

        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-logging' => 'enabled' ]);
        $logs = Client::getLogs($response['headers']['x-open-runtimes-log-id']);
        $errors = Client::getErrors($response['headers']['x-open-runtimes-log-id']);
        self::assertNotEmpty($response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('Debug log', $logs);
        self::assertStringContainsString('Error log', $errors);

        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-log-id' => 'customLogs' ]);
        $logs = Client::getLogs('customLogs');
        $errors = Client::getErrors('customLogs');
        self::assertSame('customLogs', $response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('Debug log', $logs);
        self::assertStringContainsString('Error log', $errors);
    }

    public function testLibrary(): void
    {
        $response = Client::execute(headers: ['x-action' => 'library'], body: '5');
        self::assertSame(200, $response['code']);

        $body = \json_decode($response['body'], true);

        self::assertSame('163', $body['todo']['userId']);
        self::assertSame('5', $body['todo']['id']);
        self::assertSame('Invest in cryptocurrency', $body['todo']['todo']);
        self::assertSame(false, $body['todo']['completed']);
    }

    public function testInvalidJson(): void
    {
        $response = Client::execute(headers: ['x-action' => 'requestBodyJson', 'content-type' => 'application/json'], body: '{"invaludJson:true}');

        self::assertSame(500, $response['code']);
        self::assertSame('', $response['body']);
        self::assertThat(Client::getErrors($response['headers']['x-open-runtimes-log-id']), self::callback(function($value) {
            $value = \strtolower($value);

            // Code=3840 is Swift code for JSON error
            return \str_contains($value, 'json') || \str_contains($value, 'code=3840');
        }), 'Contains refference to JSON validation problem');

        $response = Client::execute(headers: ['x-action' => 'requestBodyJson', 'content-type' => 'application/json'], body: '');

        self::assertSame(500, $response['code']);

        $response = Client::execute(headers: ['x-action' => 'requestBodyJson', 'content-type' => 'application/json'], body: '{}');
        self::assertSame(200, $response['code']);
        self::assertSame('{}', $response['body']);
    }

    public function testTimeout(): void
    {
        $response = Client::execute(headers: ['x-action' => 'timeout', 'x-open-runtimes-timeout' => '1']);
        self::assertSame(500, $response['code']);
        self::assertSame('', $response['body']);
        self::assertStringContainsString('Execution timed out.', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('Timeout start.', Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringNotContainsString('Timeout end.', Client::getLogs($response['headers']['x-open-runtimes-log-id']));

        $response = Client::execute(headers: ['x-action' => 'timeout', 'x-open-runtimes-timeout' => '5']);
        self::assertSame(200, $response['code']);
        self::assertSame('Successful response.', $response['body']);
        self::assertStringContainsString('Timeout start.', Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('Timeout end.', Client::getLogs($response['headers']['x-open-runtimes-log-id']));

        $response = Client::execute(headers: ['x-action' => 'timeout', 'x-open-runtimes-timeout' => 'abcd']);
        self::assertSame(500, $response['code']);
        self::assertSame('Header "x-open-runtimes-timeout" must be an integer greater than 0.', $response['body']);
    }

    public function testDeprecatedMethods(): void
    {
        $response = Client::execute(body: 'Hello', headers: ['x-action' => 'deprecatedMethods']);
        self::assertSame(200, $response['code']);
        self::assertSame('Hello', $response['body']);

        $response = Client::execute(body: '{"hello":"world"}', headers: ['x-action' => 'deprecatedMethods', 'content-type' => 'application/json']);
        self::assertSame(200, $response['code']);
        self::assertSame('{"hello":"world"}', $response['body']);

        $response = Client::execute(body: '{"hello":"world"}', headers: ['x-action' => 'deprecatedMethods', 'content-type' => 'application/unknown']);
        self::assertSame(200, $response['code']);
        self::assertSame('{"hello":"world"}', $response['body']);
    }

    public function testDeprecatedMethodsUntypedBody(): void
    {
        $response = Client::execute(body: 'Hello', headers: ['x-action' => 'deprecatedMethodsUntypedBody']);
        self::assertSame(200, $response['code']);
        self::assertIsString($response['body']);
        self::assertSame('50', $response['body']);
    }

    public function testDeprecatedMethodsBytesBody(): void
    {
        $response = Client::execute(body: 'Hello', headers: ['x-action' => 'deprecatedMethodsBytesBody']);

        self::assertSame(200, $response['code']);
        self::assertIsString($response['body']);
        self::assertStringStartsWith('image/png', $response['headers']['content-type']);
        self::assertSame('2a8fdeea08e939e9a7c05653544a1374', \md5($response['body']));
    }

    public function testBinaryResponse(): void
    {
        $response = Client::execute(body: '', headers: ['x-action' => 'binaryResponse1']);
        self::assertSame(200, $response['code']);
        $bytes = \unpack('C*byte', $response['body']);
        self::assertCount(3, $bytes);
        self::assertSame(0, $bytes['byte1']);
        self::assertSame(10, $bytes['byte2']);
        self::assertSame(255, $bytes['byte3']);

        $response = Client::execute(body: '', headers: ['x-action' => 'binaryResponse2']);
        self::assertSame(200, $response['code']);
        $bytes = \unpack('C*byte', $response['body']);
        self::assertCount(3, $bytes);
        self::assertSame(0, $bytes['byte1']);
        self::assertSame(20, $bytes['byte2']);
        self::assertSame(255, $bytes['byte3']);

        $response = Client::execute(body: '', headers: ['x-action' => 'binaryResponse3']);
        self::assertSame(200, $response['code']);
        $bytes = \unpack('C*byte', $response['body']);
        self::assertCount(3, $bytes);
        self::assertSame(0, $bytes['byte1']);
        self::assertSame(30, $bytes['byte2']);
        self::assertSame(255, $bytes['byte3']);


        $response = Client::execute(body: '', headers: ['x-action' => 'binaryResponse4']);
        self::assertSame(200, $response['code']);
        $bytes = \unpack('C*byte', $response['body']);
        self::assertCount(3, $bytes);
        self::assertSame(0, $bytes['byte1']);
        self::assertSame(40, $bytes['byte2']);
        self::assertSame(255, $bytes['byte3']);

        $response = Client::execute(body: '', headers: ['x-action' => 'binaryResponse5']);
        self::assertSame(200, $response['code']);
        $bytes = \unpack('C*byte', $response['body']);
        self::assertCount(3, $bytes);
        self::assertSame(0, $bytes['byte1']);
        self::assertSame(50, $bytes['byte2']);
        self::assertSame(255, $bytes['byte3']);
    }

    public function testBinaryResponseLarge(): void
    {
        $body = \file_get_contents(__DIR__.'/resources/large-file-17mb.zip');
        $md5 = \md5($body);
        $base64 = \base64_encode($body);

        $response = Client::execute(body: $body, headers: ['x-action' => 'binaryResponseLarge'], method: "PUT");
        self::assertSame(200, $response['code']);
        $this->assertThat(
            $response['body'],
            $this->logicalOr(
                $this->equalTo($md5),
                $this->equalTo($base64),
            ),
        );
        self::assertSame('PUT', $response['headers']['x-method']);

        $response = Client::execute(body: $body, headers: ['x-action' => 'binaryResponseLarge'], method: "POST");
        self::assertSame(200, $response['code']);
        $this->assertThat(
            $response['body'],
            $this->logicalOr(
                $this->equalTo($md5),
                $this->equalTo($base64),
            ),
        );
        self::assertSame('POST', $response['headers']['x-method']);

        $body = \file_get_contents(__DIR__.'/resources/large-file-23mb.zip');
        $md5 = \md5($body);

        $response = Client::execute(body: $body, headers: ['x-action' => 'binaryResponseLarge'], method: "PUT");
        $this->assertThat(
            $response['code'],
            $this->logicalOr(
                // Only allow 413 if status code becomes important; also ensure body and logs if only one code is used
                $this->equalTo(500),
                $this->equalTo(413),
            ),
        );

        $response = Client::execute(body: $body, headers: ['x-action' => 'binaryResponseLarge'], method: "POST");
        $this->assertThat(
            $response['code'],
            $this->logicalOr(
                // Only allow 413 if status code becomes important; also ensure body and logs if only one code is used
                $this->equalTo(500),
                $this->equalTo(413),
            ),
        );

        // Ensure empty bodies remain functional
        $response = Client::execute(headers: ['x-action' => 'plaintextResponse', 'content-length' => '0'], method: 'GET', body: NULL);
        self::assertSame(200, $response['code']);

        $response = Client::execute(headers: ['x-action' => 'plaintextResponse', 'content-length' => ''], method: 'GET', body: NULL);
        self::assertSame(200, $response['code']);

        $response = Client::execute(headers: ['x-action' => 'plaintextResponse', 'content-length' => NULL], method: 'GET', body: NULL);
        self::assertSame(200, $response['code']);

        $response = Client::execute(headers: ['x-action' => 'plaintextResponse'], method: 'GET', body: NULL);
        self::assertSame(200, $response['code']);
    }

    function testEnforcedHeaders(): void
    {
        $response = Client::execute(headers: ['x-action' => 'enforcedHeaders'], method: "POST");
        self::assertSame(200, $response['code']);
        self::assertNotEmpty($response['body']);

        $body = \json_decode($response['body'], true);
        self::assertSame("value", $body['x-custom']);
        self::assertSame("Value2", $body['x-custom-uppercase']);
        self::assertSame("248", $body['x-open-runtimes-custom']);

        $response = Client::execute(headers: ['x-action' => 'enforcedHeaders', 'x-custom' => 'IS_IGNORED', 'x-custom-uppercase' => 'IS_IGNORED', 'x-open-runtimes-custom' => 'IS_IGNORED'], method: "POST");
        self::assertSame(200, $response['code']);
        self::assertNotEmpty($response['body']);

        $body = \json_decode($response['body'], true);
        self::assertSame("value", $body['x-custom']);
        self::assertSame("Value2", $body['x-custom-uppercase']);
        self::assertSame("248", $body['x-open-runtimes-custom']);

        $response = Client::execute(headers: ['x-action' => 'enforcedHeaders', 'X-CUSTOM-UPPERCASE' => 'IS_IGNORED'], method: "POST");
        self::assertSame(200, $response['code']);
        self::assertNotEmpty($response['body']);

        $body = \json_decode($response['body'], true);
        self::assertSame("value", $body['x-custom']);
        self::assertSame("Value2", $body['x-custom-uppercase']);
        self::assertSame("248", $body['x-open-runtimes-custom']);
    }

    function testStartLogs(): void
    {
        $response = Client::execute(headers: ['x-action' => 'plaintextResponse']);
        self::assertSame(200, $response['code']);

        $msg = "HTTP server successfully started!";
        $response = \shell_exec('docker logs open-runtimes-test-serve');
        self::assertStringContainsString($msg, $response);
    }

    function testSystemCommands(): void
    {
        $response = Client::execute(headers: ['x-action' => 'plaintextResponse']);
        self::assertSame(200, $response['code']);

        // script from Linux utils to allow log watching
        $response = \shell_exec('docker exec open-runtimes-test-serve sh -c "script --help"');
        self::assertStringContainsString("Usage", $response);
    }

    function assertEqualsIgnoringWhitespace($expected, $actual, $message = '') {
        $expected = preg_replace('/\s+/', '', $expected);
        $actual = preg_replace('/\s+/', '', $actual);
        self::assertSame($expected, $actual, $message);
    }

    public function testSpreadOperatorLogs(): void
    {
        $response = Client::execute(body: '', headers: ['x-action' => 'spreadOperatorLogs']);

        self::assertSame(200, $response['code']);
        self::assertSame('OK', $response['body']);
        self::assertStringContainsString('engine:', Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('open-runtimes', Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString(' ', Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        $spaceOccurances = \substr_count(Client::getLogs($response['headers']['x-open-runtimes-log-id']), ' ');
        self::assertSame(1, $spaceOccurances);
        self::assertStringContainsString('engine:', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('open-runtimes', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString(' ', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
        $spaceOccurances = \substr_count(Client::getErrors($response['headers']['x-open-runtimes-log-id']), ' ');
        self::assertSame(1, $spaceOccurances);
    }

    public function testDevLogFiles(): void
    {
        Client::$host = 'open-runtimes-test-serve-secondary';

        // Cleanup
        $response = \shell_exec('rm -rf /tmp/logs/dev_logs.log && echo $?');
        self::assertStringEndsWith("0\n", $response); // Exit code 0 means success
        $response = \shell_exec('rm -rf /tmp/logs/dev_errors.log && echo $?');
        self::assertStringEndsWith("0\n", $response); // Exit code 0 means success

        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-logging' => 'disabled' ]);
        $logs = Client::getLogs('dev');
        $errors = Client::getErrors('dev');
        self::assertEmpty($response['headers']['x-open-runtimes-log-id']);
        self::assertEmpty($logs);
        self::assertEmpty($errors);

        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-logging' => 'enabled' ]);
        $logs = Client::getLogs('dev');
        $errors = Client::getErrors('dev');
        self::assertSame('dev', $response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('Debug log', $logs);
        self::assertStringContainsString('Error log', $errors);

        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-logging' => 'enabled', 'x-open-runtimes-log-id' => 'myLog' ]);
        $logs = Client::getLogs('myLog');
        $errors = Client::getErrors('myLog');
        self::assertSame('myLog', $response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('Debug log', $logs);
        self::assertStringContainsString('Error log', $errors);

        Client::$host = 'open-runtimes-test-serve';
    }
    
    public function testSetCookie(): void
    {   
        $response = Client::execute(headers: ['x-action' => 'setCookie']);
        self::assertSame(200, $response['code']);
        self::assertSame('cookie=value; path=/', $response['headers']['set-cookie'][0]);
        self::assertSame('cookie2=value2; path=/', $response['headers']['set-cookie'][1]);
        self::assertSame('some-value', $response['headers']['some-header']);
        
        $response = Client::execute(headers: ['x-action' => 'setCookie2']);
        self::assertSame(200, $response['code']);
        self::assertSame('cookie=value; path=/', $response['headers']['set-cookie'][0]);
        self::assertSame('cookie2=value2; path=/', $response['headers']['set-cookie'][1]);
        self::assertSame('some-value', $response['headers']['some-header']);
    }
}
