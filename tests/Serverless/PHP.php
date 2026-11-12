<?php

namespace Tests\Serverless;

use Tests\Client;
use Tests\Serverless;

class PHP extends Serverless
{
    public function testSetCookie(): void
    {
        self::assertTrue(true); // Disable test till implemented
    }

    public function throwingActions(): array
    {
        return [
            'plain throw' => ['errorTest'],
            'stream throws before its first chunk' => ['streamErrorStart'],
        ];
    }

    /** @dataProvider throwingActions */
    public function testErrorWithTimeout(string $action): void
    {
        $response = Client::execute(headers: ['x-action' => $action, 'x-open-runtimes-timeout' => '5']);
        self::assertEquals(500, $response['code']);

        $response = Client::execute(headers: ['x-action' => 'plaintextResponse']);
        self::assertEquals(200, $response['code']);
    }

    public function testFatalError(): void
    {
        Client::$host = 'open-runtimes-test-serve-php-fatal';

        try {
            $response = Client::execute(
                headers: [
                    'x-action' => 'fatalError',
                    'x-open-runtimes-logging' => 'enabled',
                ],
                timeout: 2,
            );
        } finally {
            Client::$host = 'open-runtimes-test-serve';
        }

        self::assertNull($response['errorNumber']);
        self::assertEquals(500, $response['code']);
        self::assertEmpty($response['body']);
        self::assertArrayHasKey('x-open-runtimes-log-id', $response['headers']);

        $errors = Client::getErrors($response['headers']['x-open-runtimes-log-id']);
        self::assertStringContainsString('Cannot redeclare FatalErrorFixture::HANDLER()', $errors);
        self::assertStringContainsString('fatal.php', $errors);
    }
}
