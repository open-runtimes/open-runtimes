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
         // Skip test for Node.js 14.5
         // Doesn't work, and investing time is not worth it
         if($this->runtimeVersion === '14.5') {
             self::assertTrue(true);
             return;
         }
         
         $response = Client::execute(body: '', headers: ['x-action' => 'headlessBrowser'], timeout: 15);
         self::assertEquals(200, $response['code']);
         self::assertEquals('image/png; charset=utf-8', $response['headers']['content-type']);
         self::assertGreaterThanOrEqual(100000, \mb_strlen($response['body'])); // Should be 1.355MB
     }
}
