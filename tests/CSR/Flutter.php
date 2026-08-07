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

    // Flutter web is a CSR SPA, so the injected value is baked into the
    // compiled bundle, not the raw `/` HTML.
    public function testDartDefinesInjected(): void
    {
        $response = Client::execute(url: '/main.dart.js', method: 'GET');
        self::assertEquals(200, $response['code']);

        // dart_defines.json fixture wins over Appwrite's OPEN_RUNTIMES_BUILD_VARS
        // on the conflicting TEST_DART_DEFINE key.
        self::assertStringContainsString('fixture_override', $response['body']);
        self::assertStringContainsString('custom_value_from_fixture', $response['body']);
        self::assertStringNotContainsString('hello_open_runtimes', $response['body']);
    }
}
