<?php

namespace Tests;

use Utopia\WebSocket;

class Websockets extends Base
{
    private WebSocket\Server $server;
    private WebSocket\Client $client;
    
    protected function setUp(): void
    {
        parent::setUp();
        
        $adapter = new WebSocket\Adapter\Swoole();
        $adapter->setPackageMaxLength(64000);
        
        $this->server = new WebSocket\Server($adapter);
        $this->setupServer();
    }

    private function setupServer(): void 
    {
        $this->server->onStart(function () {
            self::assertTrue(true, 'Server started successfully');
        });

        $this->server->onOpen(function (int $connection, $request) {
            self::assertIsInt($connection, 'Connection ID should be integer');
            self::assertNotNull($request, 'Request object should not be null');
        });

        $this->server->onMessage(function (int $connection, string $message) {
            self::assertIsString($message, 'Message should be string');
        });
    }

    public function testServerStart(): void
    {
        $started = false;
        $this->server->onStart(function () use (&$started) {
            $started = true;
        });
        
        $this->server->start();
        self::assertTrue($started, 'Server should have started');
    }

    public function testClientConnection(): void
    {
        $connected = false;
        $this->server->onOpen(function (int $connection, $request) use (&$connected) {
            $connected = true;
        });

        $client = new WebSocket\Client('ws://localhost:3000');
        $client->connect();
        
        self::assertTrue($connected, 'Client should have connected successfully');
        $client->close();
    }

    public function testMessageExchange(): void
    {
        $messageReceived = '';
        $this->server->onMessage(function (int $connection, string $message) use (&$messageReceived) {
            $messageReceived = $message;
        });

        $client = new WebSocket\Client('ws://localhost:3000');
        $client->connect();
        $testMessage = 'Hello WebSocket!';
        $client->send($testMessage);
        
        // Wait for message processing
        usleep(100000); // 100ms
        
        self::assertEquals($testMessage, $messageReceived, 'Server should receive the exact message sent by client');
        $client->close();
    }

    public function testConnectionClosure(): void
    {
        $closed = false;
        $this->server->onClose(function (int $connection) use (&$closed) {
            $closed = true;
        });

        $client = new WebSocket\Client('ws://localhost:3000');
        $client->connect();
        $client->close();
        
        self::assertTrue($closed, 'Connection closure should be detected');
    }

    public function testMaxPackageLength(): void
    {
        $adapter = new WebSocket\Adapter\Swoole();
        $maxLength = 1024;
        $adapter->setPackageMaxLength($maxLength);
        
        $server = new WebSocket\Server($adapter);
        $error = false;
        
        $server->error(function () use (&$error) {
            $error = true;
        });

        $client = new WebSocket\Client('ws://localhost:3000');
        $client->connect();
        
        $oversizedMessage = str_repeat('a', $maxLength + 1);
        $client->send($oversizedMessage);
        
        self::assertTrue($error, 'Server should detect message exceeding max package length');
        $client->close();
    }

    public function testMultipleClients(): void
    {
        $connections = [];
        $this->server->onOpen(function (int $connection, $request) use (&$connections) {
            $connections[] = $connection;
        });

        $clients = [];
        for ($i = 0; $i < 3; $i++) {
            $clients[] = new WebSocket\Client('ws://localhost:3000');
            $clients[$i]->connect();
        }
        
        self::assertCount(3, $connections, 'Server should handle multiple client connections');
        
        foreach ($clients as $client) {
            $client->close();
        }
    }

    protected function tearDown(): void
    {
        parent::tearDown();
        // Cleanup and stop server
        if (isset($this->server)) {
            $this->server->shutdown();
        }
    }
}