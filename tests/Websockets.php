<?php

namespace Tests;

class Websockets extends Base
{
    public function setUp(): void
    {
        Client::$port = 3000;
        $this->awaitPortOpen();
    }

    public function testWebsocketConnection(): void
    {
        $response = Client::execute(headers: ['x-action' => 'websocketConnection']);
        fwrite(STDOUT, "Response: " . json_encode($response, JSON_PRETTY_PRINT) . "\n");
        self::assertEquals(200, $response['code']);
        $body = json_decode($response['body'], true);
        self::assertTrue($body['success']);
        self::assertEquals('WebSocket connected successfully', $body['message']);
        self::assertEquals('application/json', $response['headers']['content-type']);
    }
}