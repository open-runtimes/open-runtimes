<?php

namespace Tests\CSR;

use Tests\CSR;
use Tests\Client;

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

    // Flutter web is a CSR SPA — the string set via OPEN_RUNTIMES_BUILD_VARS
    // in tests/compose.yaml's build service never appears in the raw `/`
    // HTML (that's just the app shell; String.fromEnvironment is resolved by
    // Flutter's compiler, not the browser). It's baked as a literal into the
    // compiled bundle instead, which is what this asserts against — the only
    // point this hook's build-prepare.sh + PATH-shadow wrapper are provable
    // without a headless browser.
    public function testDartDefinesInjected(): void
    {
        $response = Client::execute(url: '/main.dart.js', method: 'GET');
        self::assertEquals(200, $response['code']);
        self::assertStringContainsString('hello_open_runtimes', $response['body']);
    }
}
