<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class BaseDev extends TestCase
{
    public function setUp(): void
    {
        Client::$port = 3001;
    }

    public function tearDown(): void
    {
    }
}