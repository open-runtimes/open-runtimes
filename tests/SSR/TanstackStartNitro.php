<?php

namespace Tests\SSR;

use Tests\Client;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class TanstackStartNitro extends TanstackStart
{
    // Skip static cache test. In Nitro (standalone builds) we dont have control over
    // over how static files are handeled (with or without cache)
    public function testStaticCache(): void
    {
        self::assertTrue(true);
    }
}
