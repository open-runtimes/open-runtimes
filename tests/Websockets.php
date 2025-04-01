<?php

namespace Tests;

use Utopia\WebSocket;
use PHPUnit\Framework\TestCase;

class Websockets extends TestCase
{
    public WebSocket\Server $server;
    public WebSocket\Client $client;
    
    public function setUp(): void
    {   
        $adapter = new WebSocket\Adapter\Swoole();
        $adapter->setPackageMaxLength(64000);
        
        $this->server = new WebSocket\Server($adapter);
        $this->client = new WebSocket\Client('ws://localhost:3000/terminal');
        $this->client->connect();
    }

    public function testClientConnection(): void
    {
        fwrite(STDOUT, "Sending ping\n");
        $this->client->send('ping');
        $this->assertEquals('pong', $this->client->receive());
        $this->assertEquals(true, $this->client->isConnected());
    }
}