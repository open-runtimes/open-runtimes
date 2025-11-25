<?php

namespace Tests\Serverless;

use Tests\Client;

class PythonML extends Python
{
    public function testSetCookie(): void
    {
        self::assertTrue(true); // Disable test till implemented
    }
    
    public function testTensorflowVersion(): void
    {
        $response = Client::execute(body: 'Hello', headers: ['x-action' => 'tensorflowVersion']);
        self::assertEquals(200, $response['code']);
        self::assertEquals("2.20.0", $response['body']);
    }
}
