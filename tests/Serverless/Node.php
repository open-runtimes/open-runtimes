<?php

namespace Tests\Serverless;

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
}
