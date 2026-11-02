<?php

namespace Tests\SSR;

use Tests\Base;
use Tests\Client;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class Monorepo extends Base
{
    public function testWorkspacePackages(): void
    {
        $response = Client::execute(url: '/', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('UI with Workspace greeting', $response['body']);
    }
}
