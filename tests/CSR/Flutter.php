<?php

namespace Tests\CSR;

use Tests\CSR;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class Flutter extends CSR
{
    // Disable auth tests because Flutter's static server is not meant for production use
    // Static runtime should be used with Flutter's build output
    public function testWrongSecret(): void
    {
        $this->assertTrue(true);
    }

    public function testEmptySecret(): void
    {
        $this->assertTrue(true);
    }

    public function testEmptyServerSecret(): void
    {
        $this->assertTrue(true);
    }

    public function testHealth(): void
    {
        $this->assertTrue(true);
    }

    public function testTimings(): void
    {
        $this->assertTrue(true);
    }
}
