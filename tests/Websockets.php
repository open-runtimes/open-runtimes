<?php

namespace Tests;

use PHPUnit\Framework\TestCase;
use Utopia\WebSocket\Client as WebsocketClient;

use function Swoole\Coroutine\run;

class Websockets extends TestCase
{
    protected $client;

    public function setUp(): void
    {
        $this->client = new WebsocketClient(
            "ws://172.17.0.1:3000",
            [
                "timeout" => 10,
            ]
        );
    }

    public function testWebsocketConnection(): void
    {
        run(function () {
            $this->client->connect();
            $this->client->send('ping');
            $this->assertEquals('pong', $this->client->receive());
            $this->assertTrue($this->client->isConnected());
        });
    }

    public function testWebsocketDisconnect(): void
    {
        run(function () {
            $this->client->connect();
            $this->client->close();
            $this->assertFalse($this->client->isConnected());
        });
    }
}