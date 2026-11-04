<?php

namespace Tests\Serverless;

use Tests\Serverless;

class Rust extends Serverless
{
    public function testRawRequest(): void
    {
        $this->markTestSkipped('context.req.raw needs a new release of the external types package first.');
    }
}
