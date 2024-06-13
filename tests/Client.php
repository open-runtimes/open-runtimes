<?php

namespace Tests;

class Client {
    public static function execute($body = '', $url = '/', $method = 'POST', $headers = [], $port = 3000) {
        $ch = \curl_init();

        $headers = \array_merge([
            'content-type' => 'text/plain',
            'x-open-runtimes-secret' => \getenv('OPEN_RUNTIMES_SECRET')
        ], $headers);

        if(isset($callback)) {
            $headers[] = 'accept: text/event-stream';
        }

        $headersParsed = [];

        foreach ($headers as $header => $value) {
            $headersParsed[] = $header . ': ' . $value;
        }

        $responseHeaders = [];
        $optArray = [
            CURLOPT_URL => 'http://localhost:' . $port . $url,
            CURLOPT_HEADERFUNCTION => function ($curl, $header) use (&$responseHeaders) {
                $len = strlen($header);
                $header = explode(':', $header, 2);
                if (count($header) < 2) // ignore invalid headers
                    return $len;
        
                $key = strtolower(trim($header[0]));
                $responseHeaders[$key] = trim($header[1]);

                if(\in_array($key, ['x-open-runtimes-logs', 'x-open-runtimes-errors'])) {
                    $responseHeaders[$key] = \urldecode($responseHeaders[$key]);
                }
        
                return $len;
            },
            CURLOPT_CUSTOMREQUEST => $method,
            CURLOPT_POSTFIELDS => \is_array($body) ? \json_encode($body, JSON_FORCE_OBJECT) : $body,
            CURLOPT_HEADEROPT => \CURLHEADER_UNIFIED,
            CURLOPT_HTTPHEADER => $headersParsed,
            CURLOPT_TIMEOUT => 5
        ];

        if(isset($callback)) {
            $handleEvent = function ($ch, $data) use ($callback) {
                $callback($data);
                return \strlen($data);
            };

            $optArray[CURLOPT_WRITEFUNCTION] = $handleEvent;
        } else {
            $optArray[CURLOPT_RETURNTRANSFER] = true;
        }
        
        \curl_setopt_array($ch, $optArray);

        $body = curl_exec($ch);

        $code = curl_getinfo($ch, \CURLINFO_HTTP_CODE);

        if (curl_errno($ch)) {
            \var_dump(curl_error($ch));
        }

        \curl_close($ch);

        return [
            'code' => $code,
            'body' => $body,
            'headers' => $responseHeaders
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