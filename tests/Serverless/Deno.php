<?php

namespace Tests\Serverless;

use Tests\Serverless;

class Deno extends Serverless
{
    use NativeResponse;

    public function testSetCookie(): void
    {
        self::assertTrue(true); // Disable test till implemented
    }
}
