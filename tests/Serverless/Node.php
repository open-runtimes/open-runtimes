<?php

namespace Tests\Serverless;

use Tests\Client;
use Tests\Serverless;

class Node extends Serverless
{

    /**
     * TODO: In future, when this becomes important,
     * Move to Serverless.php and update all runtime tests to have action for this
     * 
     * public function testHiddenFile(): void
     * {
     *   $response = Client::execute(body: '', headers: ['x-action' => 'hiddenFile']);
     *   self::assertEquals(200, $response['code']);
     *   self::assertEquals('HIDDEN_FILE', $response['body']);
     * }
     */
     
     public function testHeadlessBrowser(): void
     {
         $response = Client::execute(body: '', headers: ['x-action' => 'headlessBrowser']);
         \var_dump($response);
         \var_dump(Client::getLogs($response['headers']['x-open-runtimes-log-id']));
         \var_dump(Client::getErrors($response['headers']['x-open-runtimes-log-id']));
         self::assertEquals(200, $response['code']);
         self::assertEquals('image/png', $response['headers']['content-type']);
         self::assertGreaterThanOrEqual(10000, \mb_strlen($response['body']));
     }
}
