<?php

namespace Tests\Websockets;

use Tests\Websockets;

use function Swoole\Coroutine\run;

class Workspace extends Websockets
{
    public function testTerminalOperations(): void
    {
        run(function () {
            $this->client->connect();
            
            // Test terminal size update
            $message = [
                'type' => 'terminal',
                'operation' => 'updateSize',
                'params' => [
                    'cols' => 80,
                    'rows' => 24
                ]
            ];
            $this->client->send(json_encode($message));
            
            // Test terminal command creation
            $message = [
                'type' => 'terminal',
                'operation' => 'createCommand',
                'params' => [
                    'command' => 'ls -la'
                ]
            ];
            $this->client->send(json_encode($message));
            
            // Test terminal input
            $message = [
                'type' => 'terminal_input',
                'data' => 'echo "hello world"'
            ];
            $this->client->send(json_encode($message));
            
            // Verify we receive terminal output
            $response = $this->client->receive();
            $this->assertNotEmpty($response);
            
            $this->client->close();
        });
    }

    public function testFilesystemOperations(): void
    {
        run(function () {
            $this->client->connect();
            
            // Test file creation
            $message = [
                'type' => 'fs',
                'operation' => 'createFile',
                'requestId' => 'test1',
                'params' => [
                    'filepath' => 'test.txt',
                    'content' => 'Hello World'
                ]
            ];
            $this->client->send(json_encode($message));
            $response = json_decode($this->client->receive(), true);
            $this->assertEquals('test1', $response['requestId']);
            
            // Test get file
            $message = [
                'type' => 'fs',
                'operation' => 'getFile',
                'requestId' => 'test2',
                'params' => [
                    'filepath' => 'test.txt'
                ]
            ];
            $this->client->send(json_encode($message));
            $response = json_decode($this->client->receive(), true);
            $this->assertEquals('test2', $response['requestId']);
            
            // Test update file
            $message = [
                'type' => 'fs',
                'operation' => 'updateFile',
                'requestId' => 'test3',
                'params' => [
                    'filepath' => 'test.txt',
                    'content' => 'Updated content'
                ]
            ];
            $this->client->send(json_encode($message));
            $response = json_decode($this->client->receive(), true);
            $this->assertEquals('test3', $response['requestId']);
            
            // Test create folder
            $message = [
                'type' => 'fs',
                'operation' => 'createFolder',
                'requestId' => 'test4',
                'params' => [
                    'folderpath' => 'testfolder'
                ]
            ];
            $this->client->send(json_encode($message));
            $response = json_decode($this->client->receive(), true);
            $this->assertEquals('test4', $response['requestId']);
            
            // Test delete file
            $message = [
                'type' => 'fs',
                'operation' => 'deleteFile',
                'requestId' => 'test5',
                'params' => [
                    'filepath' => 'test.txt'
                ]
            ];
            $this->client->send(json_encode($message));
            $response = json_decode($this->client->receive(), true);
            $this->assertEquals('test5', $response['requestId']);
            
            // Test delete folder
            $message = [
                'type' => 'fs',
                'operation' => 'deleteFolder',
                'requestId' => 'test6',
                'params' => [
                    'folderpath' => 'testfolder'
                ]
            ];
            $this->client->send(json_encode($message));
            $response = json_decode($this->client->receive(), true);
            $this->assertEquals('test6', $response['requestId']);
            
            $this->client->close();
        });
    }

    public function testSystemOperations(): void
    {
        run(function () {
            $this->client->connect();
            
            // Test system usage
            $message = [
                'type' => 'system',
                'operation' => 'getUsage',
                'requestId' => 'test1'
            ];
            $this->client->send(json_encode($message));
            $response = json_decode($this->client->receive(), true);
            $this->assertEquals('test1', $response['requestId']);
            
            $this->client->close();
        });
    }
}
