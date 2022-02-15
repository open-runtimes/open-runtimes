<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

ini_set('display_errors', 1);
ini_set('display_startup_errors', 1);
error_reporting(E_ALL);

class RuntimeTest extends TestCase
{
    public function testRuntimeExample(): void
    {
        $ch = \curl_init();

        $requestBody = [];

        $optArray = array(
            CURLOPT_URL => 'http://172.17.0.1:3000', // Docker loopback address to host machine's localhost
            CURLOPT_RETURNTRANSFER => true,
            CURLOPT_POST => true,
            CURLOPT_POSTFIELDS => \json_encode($requestBody),
            CURLOPT_HEADEROPT => \CURLHEADER_UNIFIED,
            CURLOPT_HTTPHEADER => array('Content-Type: application/json')
        );
        
        \curl_setopt_array($ch, $optArray);

        $result = curl_exec($ch);
        $response = curl_getinfo($ch, \CURLINFO_HTTP_CODE);

        \curl_close($ch);
        
        \var_dump("Q" . $result);
        \var_dump("E" . $response);
        
        self::assertEquals(200, $response);
        self::assertNotNull($result);

    }
}