<?php

namespace Tests;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class Flutter extends Base
{
    public function setUp(): void
    {
        parent::setUp();
    }

    public function testStaticFile(): void
    {
        // TODO: Finish
        self::assertTrue(false);
        // $response = Client::execute(url: '/index.html', method: 'GET');
    }

}
