<?php

namespace Tests\SSR;

use Tests\Client;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

/**
 * A Django project answers the shared Serverless actions (tests/resources/functions/django).
 *
 * It runs on the same WSGI layer as Flask, found from manage.py the way `runserver`
 * finds it, so it keeps the Flask suite. Overrides cover Django's own formats.
 */
class Django extends Flask
{
    public function testException(): void
    {
        // The traceback names the project's code (config/views.py); the entrypoint
        // is manage.py, which only points at the project
        $response = Client::execute(headers: ['x-action' => 'nonExistingAction']);
        self::assertEquals(500, $response['code']);
        self::assertEmpty($response['body']);
        self::assertEmpty(Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('Unknown action', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('config/views.py', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }

    public function testSetCookie(): void
    {
        // Django writes cookies itself, with a capital `Path`
        foreach (['setCookie', 'setCookie2'] as $action) {
            $response = Client::execute(headers: ['x-action' => $action]);
            self::assertEquals(200, $response['code']);
            self::assertEquals('cookie=value; Path=/', $response['headers']['set-cookie'][0]);
            self::assertEquals('cookie2=value2; Path=/', $response['headers']['set-cookie'][1]);
            self::assertEquals('some-value', $response['headers']['some-header']);
        }
    }
}
