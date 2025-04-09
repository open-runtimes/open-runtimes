<?php

namespace Tests\Workspace;

use PHPUnit\Framework\TestCase;

abstract class Workspace extends TestCase
{
    /**
     * Set up method called by PHPUnit before each test
     * This method will call the initialize() method that child classes must implement
     */
    public function setUp(): void
    {
        $this->initialize();
    }

    /**
     * Initialize the test environment
     */
    abstract protected function initialize(): void;

    /**
     * Execute a command via HTTP or WebSocket and return the response
     * 
     * @param array $message The message to send
     * @return array The response with 'success', 'data', 'error', etc.
     */
    abstract protected function executeCommand(array $message, bool $waitForResponse = true): array;

    /**
     * Test terminal operations
     */
    public function testTerminalOperations(): void
    {
        // test update size, no response expected
        $this->executeCommand(message: [
            'type' => 'terminal',
            'operation' => 'updateSize',
            'params' => [
                'cols' => 80,
                'rows' => 24
            ]
        ], waitForResponse: false);

        // test create command
        $this->executeCommand(message: [
            'type' => 'terminal',
            'operation' => 'createCommand',
            'params' => [
                'command' => 'touch test.txt'
            ]
        ], waitForResponse: false);
        
        // test list files
        $this->executeCommand(message: [
            'type' => 'terminal',
            'operation' => 'createCommand',
            'params' => [
                'command' => 'ls'
            ]
        ], waitForResponse: false);
    }

    /**
     * Test filesystem operations
     */
    public function testFilesystemOperations(): void
    {
        /**
         * Test for SUCCESS
         */

        // test create file
        $response = $this->executeCommand([
            'type' => 'fs',
            'operation' => 'createFile',
            'params' => [
                'filepath' => 'test.txt',
                'content' => 'Hello World',
            ]
        ]);
        $this->assertTrue($response['success']);

        // test get file
        $response = $this->executeCommand([
            'type' => 'fs',
            'operation' => 'getFile',
            'params' => [
                'filepath' => 'test.txt',
            ]
        ]);
        $this->assertTrue($response['success']);
        $this->assertEquals('Hello World', $response['data']);

        // test update file
        $response = $this->executeCommand([
            'type' => 'fs',
            'operation' => 'updateFile',
            'params' => [
                'filepath' => 'test.txt',
                'content' => 'Hello World 2'
            ]
        ]);
        $this->assertTrue($response['success']);

        // test create folder
        $response = $this->executeCommand([
            'type' => 'fs',
            'operation' => 'createFolder',
            'params' => [
                'folderpath' => 'testfolder'
            ]
        ]);
        $this->assertTrue($response['success']);

        // test delete file
        $response = $this->executeCommand([
            'type' => 'fs',
            'operation' => 'deleteFile',
            'params' => [
                'filepath' => 'test.txt'
            ]
        ]);
        $this->assertTrue($response['success']);

        // test delete folder
        $response = $this->executeCommand([
            'type' => 'fs',
            'operation' => 'deleteFolder',
            'params' => [
                'folderpath' => 'testfolder'
            ]
        ]);
        $this->assertTrue($response['success']);

        /**
         * Test for FAILURE
         */

        // test delete non-existent file
        $response = $this->executeCommand([
            'type' => 'fs',
            'operation' => 'deleteFile',
            'params' => [
                'filepath' => 'nonexistent.txt'
            ]
        ]);
        $this->assertFalse($response['success']);
        $this->assertStringContainsString('no such file or directory', $response['error']);
    }

    /**
     * Test system operations
     */
    public function testSystemOperations(): void
    {
        // test get usage
        $response = $this->executeCommand([
            'type' => 'system',
            'operation' => 'getUsage'
        ]);
        
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
    }

    /**
     * Test git operations
     */
    public function testGitOperations(): void
    {
        /**
         * Test for FAILURE scenarios first (no git repo)
         */

        // test get current branch (expected to fail initially)
        $response = $this->executeCommand([
            'type' => 'git',
            'operation' => 'getCurrentBranch'
        ]);
        $this->assertFalse($response['success']);
        $this->assertStringContainsString('not a git repository', $response['error']);

        /**
         * Test SUCCESS scenarios after initializing repo
         */

        // test git init
        $response = $this->executeCommand([
            'type' => 'git',
            'operation' => 'init'
        ]);
        $this->assertTrue($response['success']);
        
        // test set user name
        $response = $this->executeCommand([
            'type' => 'git',
            'operation' => 'setUserName',
            'params' => [
                'name' => 'John Doe'
            ]
        ]);
        $this->assertTrue($response['success']);
        
        // test set user email
        $response = $this->executeCommand([
            'type' => 'git',
            'operation' => 'setUserEmail',
            'params' => [
                'email' => 'john.doe@example.com'
            ]
        ]);
        $this->assertTrue($response['success']);
        
        // create a test file to commit
        $response = $this->executeCommand([
            'type' => 'fs',
            'operation' => 'createFile',
            'params' => [
                'filepath' => 'test.txt',
                'content' => 'Test content'
            ]
        ]);
        $this->assertTrue($response['success']);

        // test git add
        // TODO: fix git add for HTTP
        // $response = $this->executeCommand([
        //     'type' => 'git',
        //     'operation' => 'add',
        //     'params' => [
        //         'files' => ['.']
        //     ]
        // ]);
        // $this->assertTrue($response['success']);

        // test git status
        $response = $this->executeCommand([
            'type' => 'git',
            'operation' => 'status'
        ]);
        $this->assertTrue($response['success']);
        $this->assertStringContainsString('No commits yet', $response['data']);
        
        // test add remote
        $response = $this->executeCommand([
            'type' => 'git',
            'operation' => 'addRemote',
            'params' => [
                'name' => 'origin',
                'url' => 'https://github.com/user/repo.git'
            ]
        ]);
        $this->assertTrue($response['success']);

        // Test git commit
        // TODO: fix after git add is fixed
        // $response = $this->executeCommand([
        //     'type' => 'git',
        //     'operation' => 'commit',
        //     'params' => [
        //         'message' => 'Initial commit'
        //     ]
        // ]);
        // $this->assertTrue($response['success']);
    }

    /**
     * Test code operations
     */
    public function testCodeOperations(): void
    {
        // test code formatting
        $response = $this->executeCommand([
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
        ]);
        $this->assertTrue($response['success']);
        $this->assertStringContainsString('function test() {', $response['data']);
        
        // test code linting
        $response = $this->executeCommand([
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
        ]);
        $this->assertTrue($response['success']);
        $this->assertArrayHasKey('issues', $response['data']);
        $this->assertIsArray($response['data']['issues']);
    }
}