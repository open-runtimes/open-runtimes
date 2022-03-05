<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

abstract class Base extends TestCase
{
    public function setUp(): void
    {
    }

    public function tearDown(): void
    {
    }

    protected function call($body) {
        $ch = \curl_init();

        $optArray = array(
            CURLOPT_URL => 'http://localhost:3000',
            CURLOPT_RETURNTRANSFER => true,
            CURLOPT_POST => true,
            CURLOPT_POSTFIELDS => \json_encode($body, JSON_FORCE_OBJECT),
            CURLOPT_HEADEROPT => \CURLHEADER_UNIFIED,
            CURLOPT_HTTPHEADER => array('Content-Type: application/json', 'X-Internal-Challenge: ' . \getenv('INTERNAL_RUNTIME_KEY'))
        );
        
        \curl_setopt_array($ch, $optArray);

        $result = curl_exec($ch);
        $response = curl_getinfo($ch, \CURLINFO_HTTP_CODE);

        \curl_close($ch);

        $resultJson = \json_decode($result, true);

        if($response >= 500) {
            \var_dump($resultJson);
        }
        
        return [
            'code' => $response,
            'body' => $resultJson
        ];
    }

    public function testRuntimeEmptyBody(): void
    {
        $response = $this->call([
        ]);

        self::assertEquals(200, $response['code']);
        self::assertEquals(true, $response['body']['isTest']);
        self::assertEquals('Hello Open Runtimes 👋', $response['body']['message']);
        self::assertEquals('1', $response['body']['todo']['userId']);
        self::assertEquals('1', $response['body']['todo']['id']);
        self::assertEquals('delectus aut autem', $response['body']['todo']['title']);
        self::assertEquals(false, $response['body']['todo']['completed']);
    }

    public function testRuntimeEmptyObjectPayload(): void 
    {
        $response = $this->call([
            'payload' => '{}'
        ]);

        self::assertEquals(200, $response['code']);
        self::assertEquals(true, $response['body']['isTest']);
        self::assertEquals('Hello Open Runtimes 👋', $response['body']['message']);
        self::assertEquals('1', $response['body']['todo']['userId']);
        self::assertEquals('1', $response['body']['todo']['id']);
        self::assertEquals('delectus aut autem', $response['body']['todo']['title']);
        self::assertEquals(false, $response['body']['todo']['completed']);
    }

    public function testRuntimePopulatedPayload(): void 
    {
        $response = $this->call([
            'payload' => '{"id":"2"}'
        ]);
    
        self::assertEquals(200, $response['code']);
        self::assertEquals(true, $response['body']['isTest']);
        self::assertEquals('Hello Open Runtimes 👋', $response['body']['message']);
        self::assertEquals('1', $response['body']['todo']['userId']);
        self::assertEquals('2', $response['body']['todo']['id']);
        self::assertEquals('quis ut nam facilis et officia qui', $response['body']['todo']['title']);
        self::assertEquals(false, $response['body']['todo']['completed']);
    }

    public function testRuntimeHeadersAndEnv(): void 
    {
        $response = $this->call([
            'headers' => [
                'x-test-header' => 'Header secret'
            ],
            'env' => [
                'test-env' => 'Environment secret'
            ]
        ]);
    
        self::assertEquals(200, $response['code']);
        self::assertEquals(true, $response['body']['isTest']);
        self::assertEquals('Header secret', $response['body']['header']);
        self::assertEquals('Environment secret', $response['body']['env']);
    }

    public function testRuntimePayloadAndHeadersAndEnv(): void 
    {
        $response = $this->call([
            'payload' => '{"id":"2"}',
            'headers' => [
                'x-test-header' => 'Header secret'
            ],
            'env' => [
                'test-env' => 'Environment secret'
            ]
        ]);
    
        self::assertEquals(200, $response['code']);
        self::assertEquals(true, $response['body']['isTest']);
        self::assertEquals('Hello Open Runtimes 👋', $response['body']['message']);
        self::assertEquals('1', $response['body']['todo']['userId']);
        self::assertEquals('2', $response['body']['todo']['id']);
        self::assertEquals('quis ut nam facilis et officia qui', $response['body']['todo']['title']);
        self::assertEquals(false, $response['body']['todo']['completed']);
        self::assertEquals('Header secret', $response['body']['header']);
        self::assertEquals('Environment secret', $response['body']['env']);
    }

    public function testRuntimeGarbageBody(): void
    {
        $response = $this->call([
            "garbage" => "garbage"
        ]);

        self::assertEquals(200, $response['code']);
        self::assertEquals(true, $response['body']['isTest']);
        self::assertEquals('Hello Open Runtimes 👋', $response['body']['message']);
        self::assertEquals('1', $response['body']['todo']['userId']);
        self::assertEquals('1', $response['body']['todo']['id']);
        self::assertEquals('delectus aut autem', $response['body']['todo']['title']);
        self::assertEquals(false, $response['body']['todo']['completed']);
    }
}