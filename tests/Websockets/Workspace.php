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
                'type' => 'terminal',
                'operation' => 'input',
                'params' => [
                    'command' => 'echo "hello world"'
                ]
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

            /**
             * Test for SUCCESS
             */
            
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
            $this->assertTrue($response['success']);
            
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
            $this->assertTrue($response['success']);
            $this->assertEquals('Hello World', $response['data']);
            
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
            $this->assertTrue($response['success']);
            
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
            $this->assertTrue($response['success']);
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
            $this->assertTrue($response['success']);

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
            $this->assertTrue($response['success']);

            /**
             * Test for FAILURE
             */

            // Test delete non-existent file
            $message = [
                'type' => 'fs',
                'operation' => 'deleteFile',
                'requestId' => 'test7',
                'params' => [
                    'filepath' => 'nonexistent.txt'
                ]
            ];
            $this->client->send(json_encode($message));
            $response = json_decode($this->client->receive(), true);
            $this->assertEquals('test7', $response['requestId']);
            $this->assertFalse($response['success']);
            $this->assertStringContainsString('no such file or directory', $response['error']);

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
            $this->assertTrue($response['success']);
            $this->assertArrayHasKey('cpuCores', $response['data']);
            $this->assertArrayHasKey('cpuUsagePerCore', $response['data']);
            $this->assertArrayHasKey('cpuUsagePercent', $response['data']);
            $this->assertArrayHasKey('loadAverage1m', $response['data']);
            $this->assertArrayHasKey('loadAverage5m', $response['data']);
            $this->assertArrayHasKey('loadAverage15m', $response['data']);
            $this->assertArrayHasKey('memoryTotalBytes', $response['data']);
            $this->assertArrayHasKey('memoryUsedBytes', $response['data']);
            $this->assertArrayHasKey('memoryFreeBytes', $response['data']);
            $this->assertArrayHasKey('memoryUsagePercent', $response['data']);
            
            $this->client->close();
        });
    }

    public function testGitOperations(): void
    {
        run(function () {
            $this->client->connect();
            
            /**
             * Test for FAILURE scenarios first (no git repo)
             */

            // Repository not found - getCurrentBranch
            $message = [
                'type' => 'git',
                'operation' => 'getCurrentBranch',
                'requestId' => 'git1'
            ];
            $this->client->send(json_encode($message));
            $response = json_decode($this->client->receive(), true);
            $this->assertEquals('git1', $response['requestId']);
            $this->assertFalse($response['success']);
            $this->assertStringContainsString('not a git repository', $response['error']);

            /**
             * Test SUCCESS scenarios after initializing repo
             */

            // Test git init
            $message = [
                'type' => 'git',
                'operation' => 'init',
                'requestId' => 'git2'
            ];
            $this->client->send(json_encode($message));
            $response = json_decode($this->client->receive(), true);
            $this->assertEquals('git2', $response['requestId']);
            $this->assertTrue($response['success']);

            // Create a test file to commit
            $message = [
                'type' => 'fs',
                'operation' => 'createFile',
                'requestId' => 'git2.1',
                'params' => [
                    'filepath' => 'test.txt',
                    'content' => 'Test content'
                ]
            ];
            $this->client->send(json_encode($message));
            $response = json_decode($this->client->receive(), true);
            $this->assertEquals('git2.1', $response['requestId']);

            // Test git add
            $message = [
                'type' => 'git',
                'operation' => 'add',
                'requestId' => 'git3',
                'params' => [
                    'files' => ['.']
                ]
            ];
            $this->client->send(json_encode($message));
            $response = json_decode($this->client->receive(), true);
            $this->assertEquals('git3', $response['requestId']);
            $this->assertTrue($response['success']);

            $this->client->close();
        });
    }

    public function testCodeStyleOperations(): void
    {
        run(function () {
            $this->client->connect();
            
            // Test code formatting
            $message = [
                'type' => 'codeStyle',
                'operation' => 'format',
                'requestId' => 'style1',
                'params' => [
                    'code' => 'function test(){return true;}',
                    'options' => [
                        'language' => 'javascript',
                        'indent' => 2,
                        'useTabs' => false,
                        'semi' => true,
                        'singleQuote' => false,
                        'printWidth' => 80
                    ]
                ]
            ];
            $this->client->send(json_encode($message));
            $response = json_decode($this->client->receive(), true);
            $this->assertEquals('style1', $response['requestId']);
            $this->assertTrue($response['success']);
            $this->assertNotEmpty($response['data']);
            $this->assertStringContainsString('function test() {', $response['data']);

            // Test code linting
            $message = [
                'type' => 'codeStyle',
                'operation' => 'lint',
                'requestId' => 'style2',
                'params' => [
                    'code' => 'function test() { const x = 1; return x; }',
                    'options' => [
                        'language' => 'javascript',
                        'rules' => [
                            'semi' => 'error',
                            'no-unused-vars' => 'warn'
                        ]
                    ]
                ]
            ];
            $this->client->send(json_encode($message));
            $response = json_decode($this->client->receive(), true);
            $this->assertEquals('style2', $response['requestId']);
            $this->assertTrue($response['success']);
            $this->assertArrayHasKey('issues', $response['data']);
            $this->assertIsArray($response['data']['issues']);
            
            $this->client->close();
        });
    }
}
