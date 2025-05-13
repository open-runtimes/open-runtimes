<?php

namespace Tests\Workspace;

use Tests\Client;

class WorkspaceHTTPTest extends WorkspaceBase
{
    public function initialize(): void
    {
        Client::$port = 3000;
        $this->awaitPortOpen();

        Client::$port = 3001;
        $this->awaitPortOpen();
        
        Client::$port = 3002;
        $this->awaitPortOpen();

        Client::$port = 3000;

        // set workDir
        $response = $this->executeCommand([
            'type' => 'terminal',
            'operation' => 'updateWorkDir',
            'params' => [
                'workDir' => '/tmp/workspace/http-test'
            ]
        ]);

        $this->assertTrue($response['success']);
        $this->assertEquals('Work directory updated successfully', $response['data']);
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

    /**
     * Execute a command via HTTP and return the response
     * 
     * @param array $message The message to send
     * @return array The response with 'success', 'data', 'error', etc.
     */
    protected function executeCommand(array $message, bool $waitForResponse = true): array
    {
        $response = Client::execute(
            url: '/', 
            method: 'POST', 
            body: $message,
            headers: [
                'content-type' => 'application/json'
            ]
        );
        
        $responseData = json_decode($response['body'], true);
        
        // Add HTTP status code to response data
        if (isset($response['code'])) {
            $responseData['http_code'] = $response['code'];
        }
        
        return $responseData;
    }

    /**
     * Override the test health method since the HTTP implementation
     * specifically calls the /health endpoint directly
     */
    public function testHealth(): void
    {
        $response = Client::execute(url: '/health');
        $json = json_decode($response['body'], true);

        $this->assertEquals(200, $response['code']);
        $this->assertTrue($json['success']);
        $this->assertEquals('OK', $json['data']);
    }
}