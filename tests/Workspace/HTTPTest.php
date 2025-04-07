<?php

namespace Tests\Workspace;

use PHPUnit\Framework\TestCase;
use Tests\Client;

class HTTPTest extends TestCase
{
    public function setUp(): void
    {
        Client::$host = '172.17.0.1';

        Client::$port = 3000;
        $this->awaitPortOpen();

        Client::$port = 3001;
        $this->awaitPortOpen();
        
        Client::$port = 3002;
        $this->awaitPortOpen();

        Client::$port = 3000;

        // set workDir
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'synapse',
            'operation' => 'updateWorkDir',
            'params' => [
                'workdir' => '/tmp/workspace/http-test'
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);
        $this->assertEquals('Work directory updated successfully', $json['data']);
    }

    protected function awaitPortOpen() {
        $attempts = 0;
        while(true) {
            $response = Client::execute();
            if($response['code'] != 0) {
                return;
            }

            if($attempts >= 100) {
                break;
            }

            sleep(1);
            $attempts++;
        }

        // Getting here means timeout failure
        throw new \Exception("Server did not start on port :3000 within 100 seconds. Check docker container logs");
    }

    public function testHealth(): void
    {
        $response = Client::execute(url: '/health');
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);
        $this->assertEquals('OK', $json['data']);
    }

    public function testTerminalOperations(): void
    {
        // test update size
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'terminal',
            'operation' => 'updateSize',
            'params' => [
                'cols' => 80,
                'rows' => 24
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $this->assertEquals(200, $response['code']);

        // test create command
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'terminal',
            'operation' => 'createCommand',
            'params' => [
                'command' => 'touch test.txt'
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $this->assertEquals(200, $response['code']);
        
        // test list files
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'terminal',
            'operation' => 'createCommand',
            'params' => [
                'command' => 'ls'
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $this->assertEquals(200, $response['code']);
    }

    public function testFilesystemOperations(): void
    {
        /**
         * Test for SUCCESS
         */

        // test create file
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'fs',
            'operation' => 'createFile',
            'params' => [
                'filepath' => 'test.txt',
                'content' => 'Hello World',
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);
    
        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);

        // test get file
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'fs',
            'operation' => 'getFile',
            'params' => [
                'filepath' => 'test.txt',
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);
        $this->assertEquals('Hello World', $json['data']);

        // test update file
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'fs',
            'operation' => 'updateFile',
            'params' => [
                'filepath' => 'test.txt',
                'content' => 'Hello World 2'
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);

        // test create folder
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'fs',
            'operation' => 'createFolder',
            'params' => [
                'folderpath' => 'testfolder'
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);

        // test delete file
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'fs',
            'operation' => 'deleteFile',
            'params' => [
                'filepath' => 'test.txt'
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);

        // test delete folder
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'fs',
            'operation' => 'deleteFolder',
            'params' => [
                'folderpath' => 'testfolder'
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);

        /**
         * Test for FAILURE
         */

        // test delete non-existent file
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'fs',
            'operation' => 'deleteFile',
            'params' => [
                'filepath' => 'nonexistent.txt'
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(400, $response['code']);
        $this->assertFalse($json['success']);
        $this->assertStringContainsString('no such file or directory', $json['error']);
    }

    public function testSystemOperations(): void
    {
        /**
         * Test for SUCCESS
         */

        // test get usage
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'system',
            'operation' => 'getUsage'
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);

        $this->assertArrayHasKey('cpuCores', $json['data']);
        $this->assertArrayHasKey('cpuUsagePerCore', $json['data']);
        $this->assertArrayHasKey('cpuUsagePercent', $json['data']);
        $this->assertArrayHasKey('loadAverage1m', $json['data']);
        $this->assertArrayHasKey('loadAverage5m', $json['data']);
        $this->assertArrayHasKey('loadAverage15m', $json['data']);
        $this->assertArrayHasKey('memoryTotalBytes', $json['data']);
        $this->assertArrayHasKey('memoryUsedBytes', $json['data']);
        $this->assertArrayHasKey('memoryFreeBytes', $json['data']);
        $this->assertArrayHasKey('memoryUsagePercent', $json['data']);
    }

    public function testGitOperations(): void
    {
        /**
         * Test for FAILURE scenarios first (no git repo)
         */

        // test get current branch
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'git',
            'operation' => 'getCurrentBranch'
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(400, $response['code']);
        $this->assertFalse($json['success']);
        $this->assertStringContainsString('not a git repository', $json['error']);

        /**
         * Test SUCCESS scenarios after initializing repo
         */

        // test git init
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'git',
            'operation' => 'init'
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);
        
        // test set user name
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'git',
            'operation' => 'setUserName',
            'params' => [
                'name' => 'John Doe'
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);
        
        // test set user email
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'git',
            'operation' => 'setUserEmail',
            'params' => [
                'email' => 'john.doe@example.com'
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);
        
        // create a test file to commit
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'fs',
            'operation' => 'createFile',
            'params' => [
                'filepath' => 'test.txt',
                'content' => 'Test content'
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);

        // test git status
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'git',
            'operation' => 'status'
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);
        $this->assertStringContainsString('No commits yet', $json['data']);
        
        // test add remote
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'git',
            'operation' => 'addRemote',
            'params' => [
                'name' => 'origin',
                'url' => 'https://github.com/user/repo.git'
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);

        // TODO: test git add and commit
    }

    public function testCodeOperations(): void
    {
        /**
         * Test for SUCCESS
         */

        // test code formatting
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'code',
            'operation' => 'format',
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
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);
        $this->assertEquals("function test() {\n  return true;\n}\n", $json['data']);
        
        // test code linting
        $response = Client::execute(url: '/', method: 'POST', body: [
            'type' => 'code',
            'operation' => 'lint',
            'params' => [
                'code' => 'function test(){return true;}',
                'options' => [
                    'language' => 'javascript',
                    'rules' => [
                        'semi' => 'error',
                        'no-unused-vars' => 'warn'
                    ]
                ]
            ]
        ], headers: [
            'content-type' => 'application/json'
        ]);
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);
        $this->assertArrayHasKey('issues', $json['data']);
        $this->assertIsArray($json['data']['issues']);
    }
}