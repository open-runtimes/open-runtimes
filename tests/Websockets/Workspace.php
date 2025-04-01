<?php

namespace Tests\Workspace;

use Tests\Websockets;
use Tests\Client;

class Workspace extends Websockets
{
    public function testTerminalOperations(): void
    {
        $response = Client::execute(headers: ['x-action' => 'terminalOperations']);
        self::assertEquals(200, $response['code']);
        $body = json_decode($response['body'], true);
        self::assertTrue($body['success']);
        self::assertArrayHasKey('response', $body);
        self::assertEquals('terminal_response', $body['response']['type']);
        self::assertEquals('application/json', $response['headers']['content-type']);
    }

    public function testFileSystemOperations(): void
    {
        $response = Client::execute(headers: ['x-action' => 'fileSystemOperations']);
        self::assertEquals(200, $response['code']);
        $body = json_decode($response['body'], true);
        self::assertTrue($body['success']);
        self::assertArrayHasKey('response', $body);
        self::assertEquals('fs_response', $body['response']['type']);
        self::assertEquals('application/json', $response['headers']['content-type']);
    }

    public function testTerminalInputOutput(): void
    {
        $response = Client::execute(headers: ['x-action' => 'terminalInputOutput']);
        self::assertEquals(200, $response['code']);
        $body = json_decode($response['body'], true);
        self::assertTrue($body['success']);
        self::assertArrayHasKey('output', $body);
        self::assertNotEmpty($body['output']);
        self::assertEquals('application/json', $response['headers']['content-type']);
    }

    public function testSystemUsage(): void
    {
        $response = Client::execute(headers: ['x-action' => 'systemUsage']);
        self::assertEquals(200, $response['code']);
        $body = json_decode($response['body'], true);
        self::assertTrue($body['success']);
        self::assertArrayHasKey('response', $body);
        self::assertEquals('system_response', $body['response']['type']);
        self::assertEquals('application/json', $response['headers']['content-type']);
    }
}
