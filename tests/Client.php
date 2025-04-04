<?php

namespace Tests;

class Client {
    public static $host = 'localhost';
    public static $port = 3000;
    public static $secret = '';

    public static function execute($body = '', $url = '/', $method = 'POST', $headers = []) {
        $ch = \curl_init();

        $initHeaders = [];

        if(!(\array_key_exists('content-type', $headers))) {
            $initHeaders['content-type'] = 'text/plain';
        }

        if(!empty(self::$secret)) {
            $initHeaders['x-open-runtimes-secret'] = self::$secret;
            $initHeaders['Authorization'] = 'Basic ' . \base64_encode('opr:' . self::$secret);
        }

        $headers = \array_merge($initHeaders, $headers);

        $headersParsed = [];

        foreach ($headers as $header => $value) {
            $headersParsed[] = $header . ': ' . $value;
        }

        $responseHeaders = [];
        $optArray = [
            CURLOPT_URL => 'http://' . self::$host . ':' . self::$port . $url,
            CURLOPT_RETURNTRANSFER => true,
            CURLOPT_HEADERFUNCTION => function ($curl, $header) use (&$responseHeaders) {
                $len = strlen($header);
                $header = explode(':', $header, 2);
                if (count($header) < 2) // ignore invalid headers
                    return $len;
        
                $key = strtolower(trim($header[0]));
                $responseHeaders[$key] = trim($header[1]);
        
                return $len;
            },
            CURLOPT_CUSTOMREQUEST => $method,
            CURLOPT_HEADEROPT => \CURLHEADER_UNIFIED,
            CURLOPT_HTTPHEADER => $headersParsed,
            CURLOPT_TIMEOUT => 5
        ];

        if($body !== NULL) {
            $optArray[CURLOPT_POSTFIELDS] = \is_array($body) ? \json_encode($body, JSON_FORCE_OBJECT) : $body;
        }
        
        \curl_setopt_array($ch, $optArray);

        $body = curl_exec($ch);
        $code = curl_getinfo($ch, \CURLINFO_HTTP_CODE);

        $error = null;
        $errorNumber = null;
        if (curl_errno($ch)) {
            $errorNumber = curl_errno($ch);
            $error = curl_error($ch);
        }

        \curl_close($ch);

        return [
            'code' => $code,
            'body' => $body,
            'headers' => $responseHeaders,
            'error' => $error,
            'errorNumber' => $errorNumber,
        ];
    }

    public static function getErrors(string $id) {
        if(!\file_exists("/tmp/logs/{$id}_errors.log")) {
            return "";
        }

        return \file_get_contents("/tmp/logs/{$id}_errors.log");
    }

    public static function getLogs(string $id) {
        if(!\file_exists("/tmp/logs/{$id}_logs.log")) {
            return "";
        }

        return \file_get_contents("/tmp/logs/{$id}_logs.log");
    }
}