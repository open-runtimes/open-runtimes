<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

abstract class BaseV3 extends TestCase
{
    public function setUp(): void
    {
    }

    public function tearDown(): void
    {
    }

    private function execute($body = '', $url = '/', $method = 'POST', $headers = []) {
        $ch = \curl_init();

        $responseHeaders = [];
        $optArray = array(
            CURLOPT_URL => 'http://localhost:3000' . $url,
            CURLOPT_RETURNTRANSFER => true,
            CURLOPT_HEADERFUNCTION => function ($curl, $header) use (&$responseHeaders) {
                $len = strlen($header);
                $header = explode(':', $header, 2);
                if (count($header) < 2) // ignore invalid headers
                    return $len;
        
                $key = strtolower(trim($header[0]));
                $responseHeaders[$key] = trim($header[1]);

                if(\in_array($key, ['x-openruntimes-logs', 'x-openruntimes-errors'])) {
                    $responseHeaders[$key] = \urldecode($responseHeaders[$key]);
                }
        
                return $len;
            },
            CURLOPT_CUSTOMREQUEST => $method,
            CURLOPT_POSTFIELDS => \is_array($body) ? \json_encode($body, JSON_FORCE_OBJECT) : $body,
            CURLOPT_HEADEROPT => \CURLHEADER_UNIFIED,
            CURLOPT_HTTPHEADER => \array_merge($headers, array('x-openruntimes-secret: ' . \getenv('INTERNAL_RUNTIME_KEY')))
        );
        
        \curl_setopt_array($ch, $optArray);

        $body = curl_exec($ch);
        $code = curl_getinfo($ch, \CURLINFO_HTTP_CODE);

        \curl_close($ch);

        if($code === 500) {
            var_dump($code);
            var_dump($body);
            var_dump($responseHeaders);
        }
        
        return [
            'code' => $code,
            'body' => $body,
            'headers' => $responseHeaders
        ];
    }

    public function testPlaintextResponse(): void
    {
        $response = $this->execute(headers: ['x-action: plaintextResponse']);
        self::assertEquals(200, $response['code']);
        self::assertEquals('Hello World 👋', $response['body']);
    }

    public function testJsonResponse(): void
    {
        $response = $this->execute(headers: ['x-action: jsonResponse']);
        $body = \json_decode($response['body'], true);
        self::assertEquals(200, $response['code']);
        self::assertEquals(true, $body['json']);
        self::assertEquals('Developers are awesome.', $body['message']);
        self::assertEquals('application/json', $response['headers']['content-type']);
    }
}