<?php

namespace Tests\Workspace;

use Utopia\WebSocket\Client as WebsocketClient;
use function Swoole\Coroutine\run;

class WorkspaceWebsocketsTest extends WorkspaceBase
{
    protected $client;

    public function initialize(): void
    {
        $this->client = new WebsocketClient(
            "ws://172.17.0.1:3000?workDir=/tmp/workspace/websocket-test",
            [
                "timeout" => 10,
            ]
        );
    }

    /**
     * Execute a command via WebSocket and return the response
     * 
     * @param array $message The message to send
     * @return array The response with 'success', 'data', 'error', etc.
     */
    protected function executeCommand(array $message, bool $waitForResponse = true): array
    {
        // Add a requestId if not present
        if (!isset($message['requestId'])) {
            $message['requestId'] = uniqid('test_');
        }
        
        $this->client->send(json_encode($message));
        $response = [];
        if ($waitForResponse) {
            do {
                $response = json_decode($this->client->receive(), true);
            } while (isset($response['type']) && ($response['type'] === 'terminal' || $response['type'] === 'syncWorkDir'));
        }
        
        return $response;
    }

    /**
     * Test WebSocket connection with ping/pong
     */
    // public function testWebsocketConnection(): void
    // {
    //     run(function () {
    //         $this->client->connect();
    //         $this->client->send('ping'); // currently disabled because client does not support ping/pong
    //         $this->assertEquals('pong', $this->client->receive());
    //         $this->assertTrue($this->client->isConnected());
    //         $this->client->close();
    //     });
    // }

    /**
     * Override parent tests to run them within coroutines
     */
    public function testTerminalOperations(): void
    {
        run(function () {
            $this->client->connect();
            parent::testTerminalOperations();
            $this->client->close();
            $this->assertTrue(true); // temporarily added
        });
    }

    public function testFilesystemOperations(): void
    {
        run(function () {
            $this->client->connect();
            parent::testFilesystemOperations();
            $this->client->close();
        });
    }

    public function testSystemOperations(): void
    {
        run(function () {
            $this->client->connect();
            parent::testSystemOperations();
            $this->client->close();
        });
    }

    public function testGitOperations(): void
    {
        run(function () {
            $this->client->connect();
            parent::testGitOperations();
            $this->client->close();
        });
    }

    public function testCodeOperations(): void
    {
        run(function () {
            $this->client->connect();
            parent::testCodeOperations();
            $this->client->close();
        });
    }

    /**
     * Test WebSocket disconnect
     */
    public function testWebsocketDisconnect(): void
    {
        run(function () {
            $this->client->connect();
            $this->client->close();
            $this->assertFalse($this->client->isConnected());
        });
    }
}