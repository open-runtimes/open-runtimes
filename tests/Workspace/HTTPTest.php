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
        $this->assertEquals(200, $response['code']);
        $this->assertEquals('OK', $response['data']);
    }
}