<?php

namespace Tests\Serverless;

use Tests\Client;
use Tests\Serverless;

class Node extends Serverless
{
    /**
     * TODO: In future, when this becomes important,
     * Move to Serverless.php and update all runtime tests to have action for this
     * 
     * public function testHiddenFile(): void
     * {
     *   $response = Client::execute(body: '', headers: ['x-action' => 'hiddenFile']);
     *   self::assertEquals(200, $response['code']);
     *   self::assertEquals('HIDDEN_FILE', $response['body']);
     * }
     */
     
    public function testLogs(): void
    {
        $response = Client::execute(headers: ['x-action' => 'logs']);
        $logId = $response['headers']['x-open-runtimes-log-id'];
        $logs = Client::getLogs($logId);
        $errors = Client::getErrors($logId);
        self::assertEquals(20, \strlen($logId));
        self::assertEquals(200, $response['code']);
        self::assertEmpty($response['body']);

        // Native and context logs
        self::assertStringContainsString('Native log', $logs);
        self::assertStringContainsString('Native logs detected.', $logs);
        self::assertStringContainsString('Debug log', $logs);
        self::assertStringContainsString('Error log', $errors);

        // Plus symbol
        self::assertStringContainsString('Log+With+Plus+Symbol', $logs);

        // Primitives
        self::assertStringContainsString('42', $logs);
        self::assertStringContainsString('4.2', $logs);
        self::assertStringContainsString('true', \strtolower($logs));

        // Object with special types
        self::assertStringContainsString('objectKey', $logs);
        self::assertStringContainsString('objectValue', $logs);
        self::assertStringContainsString('9223372036854775807', $logs);
        self::assertStringContainsString('-9223372036854775808', $logs);
        self::assertStringContainsString('setvalue1', $logs);
        self::assertStringContainsString('setvalue2', $logs);
        self::assertStringContainsString('setvalue3', $logs);
        self::assertStringContainsString('https://', $logs);
        self::assertStringContainsString('appwrite.io', $logs);
        self::assertStringContainsString('my-awesome-path', $logs);

        // Array
        self::assertStringContainsString('arrayValue', $logs);

        // Log structure
        self::assertStringContainsString("\n", $logs);
        self::assertGreaterThanOrEqual(9, \count(\explode("\n", $logs)));
        self::assertGreaterThanOrEqual(1, \count(\explode("\n", $errors)));

        // Truncation
        self::assertStringContainsString('... Log truncated due to size limit (8000 characters)', $logs);
        self::assertStringContainsString('... Log truncated due to size limit (8000 characters)', $errors);

        // Unique log IDs
        $response = Client::execute(headers: ['x-action' => 'logs']);
        $logIdSecond = $response['headers']['x-open-runtimes-log-id'];
        self::assertEquals(20, \strlen($logIdSecond));
        self::assertNotEquals($logId, $logIdSecond);

        // Disabled logging
        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-logging' => 'disabled', 'x-open-runtimes-log-id' => 'noLogs']);
        $logs = Client::getLogs('noLogs');
        $errors = Client::getErrors('noLogs');
        self::assertEmpty($response['headers']['x-open-runtimes-log-id']);
        self::assertEmpty($logs);
        self::assertEmpty($errors);

        // Enabled logging
        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-logging' => 'enabled']);
        $logs = Client::getLogs($response['headers']['x-open-runtimes-log-id']);
        $errors = Client::getErrors($response['headers']['x-open-runtimes-log-id']);
        self::assertNotEmpty($response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('Debug log', $logs);
        self::assertStringContainsString('Error log', $errors);

        // Custom log ID
        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-log-id' => 'customLogs']);
        $logs = Client::getLogs('customLogs');
        $errors = Client::getErrors('customLogs');
        self::assertEquals('customLogs', $response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('Debug log', $logs);
        self::assertStringContainsString('Error log', $errors);
    }

     public function testHeadlessBrowser(): void
     {   
         $response = Client::execute(body: '', headers: ['x-action' => 'headlessBrowser'], timeout: 15);
         self::assertEquals(200, $response['code']);
         self::assertEquals('image/png; charset=utf-8', $response['headers']['content-type']);
         self::assertGreaterThanOrEqual(100000, \mb_strlen($response['body'])); // Should be 1.355MB
     }

     public function testRawResponse(): void
    {
        if (str_starts_with($this->runtimeVersion, '16.0')) {
            self::markTestSkipped('Node 16 does not support raw response object.');
        }

        // Text response
        $response = Client::execute(headers: ['x-action' => 'responseObjectText']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('Hello World 👋', $response['body']);
        self::assertEqualsIgnoringWhitespace('text/plain; charset=utf-8', $response['headers']['content-type']);

        // JSON response
        $response = Client::execute(headers: ['x-action' => 'responseObjectJson']);
        self::assertEquals(200, $response['code']);
        self::assertEqualsIgnoringWhitespace('application/json; charset=utf-8', $response['headers']['content-type']);

        $body = \json_decode($response['body'], true);
        self::assertEquals(true, $body['json']);
        self::assertEquals('Developers are awesome.', $body['message']);

        // Binary response
        $response = Client::execute(headers: ['x-action' => 'responseObjectBinary']);
        self::assertEquals(200, $response['code']);
        $bytes = \unpack('C*byte', $response['body']);
        self::assertCount(3, $bytes);
        self::assertEquals(0, $bytes['byte1']);
        self::assertEquals(10, $bytes['byte2']);
        self::assertEquals(255, $bytes['byte3']);

        // Empty response
        $response = Client::execute(headers: ['x-action' => 'responseObjectEmpty']);
        self::assertEquals(204, $response['code']);
        self::assertEmpty($response['body']);

        // Redirect response
        $response = Client::execute(headers: ['x-action' => 'responseObjectRedirect']);
        self::assertEquals(301, $response['code']);
        self::assertEmpty($response['body']);
        self::assertEquals('https://github.com/', $response['headers']['location']);

        // Custom status code
        $response = Client::execute(headers: ['x-action' => 'responseObjectStatus']);
        self::assertEquals(404, $response['code']);
        self::assertEquals('FAIL', $response['body']);

        // Custom headers
        $response = Client::execute(headers: ['x-action' => 'responseObjectHeaders']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('OK', $response['body']);
        self::assertEquals('first-value', $response['headers']['first-header']);
        self::assertEquals('second-value', $response['headers']['second-header']);
    }

    public function testRawRequest(): void
    {
        if (str_starts_with($this->runtimeVersion, '16.0')) {
            self::markTestSkipped('Node 16 does not support raw request object.');
        }

        $response = Client::execute(
            body: 'Hello Native',
            method: 'POST',
            headers: ['x-action' => 'rawRequest', 'content-type' => 'text/plain']
        );
        self::assertEquals(200, $response['code']);

        $body = \json_decode($response['body'], true);

        self::assertEquals('POST', $body['method']);
        self::assertEquals('Hello Native', $body['body']);
        self::assertTrue($body['hasContentType']);
        self::assertStringStartsWith('http', $body['url']);
    }
}
