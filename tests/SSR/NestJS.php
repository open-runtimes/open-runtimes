<?php

namespace Tests\SSR;

use Tests\Client;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

/**
 * A NestJS app answers the shared Serverless actions (tests/resources/functions/nestjs).
 *
 * NestJS runs on Express and the NestJS helper starts it like an Express app, so it
 * keeps the Express suite: the same routing, logs, timeouts and graceful shutdown.
 */
class NestJS extends Express
{
    public function testException(): void
    {
        // The stack names the controller that threw (dist/app.controller.js); the
        // entrypoint, dist/main.js, only starts the app
        $response = Client::execute(headers: ['x-action' => 'nonExistingAction']);
        self::assertEquals(500, $response['code']);
        self::assertEmpty($response['body']);
        self::assertEmpty(Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('Unknown action', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
        self::assertStringContainsString('dist/app.controller.js', Client::getErrors($response['headers']['x-open-runtimes-log-id']));
    }
}
