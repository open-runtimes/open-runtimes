<?php

namespace Tests\SSR;

use Tests\Client;
use Tests\Serverless;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

/**
 * A Flask app answers the shared Serverless actions (tests/resources/functions/flask/app.py).
 *
 * It runs under gunicorn, wrapped in the Open Runtimes contract by
 * src/ssr/open_runtimes_wsgi.py. Overrides cover what a WSGI app (Flask, Django) does not have:
 * the function contract (return value, context.req.raw, the function server) and
 * what the SSR runtime leaves to the app, as for Express.
 */
class Flask extends Serverless
{
    public function testRouting(): void
    {
        $response = Client::execute(url: '/users/42?sort=asc', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertEquals(['id' => '42', 'sort' => 'asc'], \json_decode($response['body'], true));
    }

    public function testNoResponse(): void
    {
        self::markTestSkipped('Function contract: a WSGI app returns its response, there is no context.res.');
    }

    public function testDoubleResponse(): void
    {
        self::markTestSkipped('Function contract: a WSGI app returns one response.');
    }

    public function testRawRequest(): void
    {
        self::markTestSkipped('Function contract: a WSGI app reads the request itself, there is no context.req.raw.');
    }

    public function testStartLogs(): void
    {
        self::markTestSkipped('Function server message: the app runs under its own WSGI server.');
    }

    public function testHeadersResponse(): void
    {
        self::markTestSkipped('SSR runtime: x-open-runtimes-* headers reach the app, as for every SSR framework.');
    }

    public function testRequestHeaders(): void
    {
        self::markTestSkipped('SSR runtime: x-open-runtimes-* headers reach the app, as for every SSR framework.');
    }

    public function testEnforcedHeaders(): void
    {
        self::markTestSkipped('SSR runtime: OPEN_RUNTIMES_HEADERS is not applied to SSR requests.');
    }

    public function testStreamTimeout(): void
    {
        self::markTestSkipped('SSR runtime: the timeout applies until the response starts; a started stream runs on.');
    }

    public function testLogs(): void
    {
        // The SSR runtime captures native logs as written, without the function
        // runtime's "Native logs detected." notice or its 8000-character truncation.
        $response = Client::execute(headers: ['x-action' => 'logs']);
        $logId = $response['headers']['x-open-runtimes-log-id'];
        $logs = Client::getLogs($logId);
        $errors = Client::getErrors($logId);
        self::assertEquals(20, \strlen($logId));
        self::assertEquals(200, $response['code']);
        self::assertEmpty($response['body']);
        self::assertStringContainsString('Native log', $logs);
        self::assertStringContainsString('Debug log', $logs);
        self::assertStringContainsString('Log+With+Plus+Symbol', $logs);
        self::assertStringContainsString('objectValue', $logs);
        self::assertStringContainsString('arrayValue', $logs);
        self::assertStringNotContainsString('Native logs detected', $logs);
        self::assertStringContainsString(\str_repeat('A,', 8999) . 'A', $logs);
        self::assertStringContainsString('Error log', $errors);

        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-logging' => 'disabled', 'x-open-runtimes-log-id' => 'noLogs']);
        self::assertEmpty($response['headers']['x-open-runtimes-log-id']);
        self::assertEmpty(Client::getLogs('noLogs'));
        self::assertEmpty(Client::getErrors('noLogs'));

        $response = Client::execute(headers: ['x-action' => 'logs', 'x-open-runtimes-log-id' => 'customLogs']);
        self::assertEquals('customLogs', $response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('Debug log', Client::getLogs('customLogs'));
        self::assertStringContainsString('Error log', Client::getErrors('customLogs'));
    }
}
