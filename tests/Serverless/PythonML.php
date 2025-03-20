<?php

namespace Tests\Serverless;

use Tests\Client;

class PythonML extends Python
{
    public function testTensorflowVersion(): void
    {
        $response = Client::execute(body: 'Hello', headers: ['x-action' => 'tensorflowVersion']);

        \var_dump($response);
        \var_dump(Client::getLogs($response['headers']['x-open-runtimes-log-id']));
        \var_dump(Client::getErrors($response['headers']['x-open-runtimes-log-id']));
        self::assertEquals(200, $response['code']);
        self::assertEquals("2.19.0", $response['body']);
    }
}
