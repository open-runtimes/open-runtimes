<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class Base extends TestCase
{
    protected string $runtimeName = '';
    protected string $runtimeVersion = '';
    
    public function setUp(): void
    {
        $this->runtimeName = \getenv('RUNTIME_NAME');
        $this->runtimeVersion = \getenv('RUNTIME_VERSION');
    }

    public function tearDown(): void
    {
    }
}
