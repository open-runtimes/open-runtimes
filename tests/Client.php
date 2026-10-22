<?php

namespace Tests;

class Client {
    public static $host = 'open-runtimes-test-serve';
    public static $port = 3000;
    public static $secret = '';

    public static function execute($body = '', $url = '/', $method = 'POST', $headers = [], int $timeout = 5) {
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
                
                if(array_key_exists($key, $responseHeaders)) {
                    if(\is_array($responseHeaders[$key])) {
                        $responseHeaders[$key][] = trim($header[1]);
                    } else {
                        $responseHeaders[$key] = [
                            $responseHeaders[$key],
                            trim($header[1])
                        ];
                    }
                } else {
                    $responseHeaders[$key] = trim($header[1]);
                }
        
                return $len;
            },
            CURLOPT_CUSTOMREQUEST => $method,
            CURLOPT_HEADEROPT => \CURLHEADER_UNIFIED,
            CURLOPT_HTTPHEADER => $headersParsed,
            CURLOPT_TIMEOUT => $timeout
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

    public static function executeChunked($body = '', $url = '/', $method = 'POST', $headers = [], int $timeout = 30, int $chunkSize = 65536) {
        $initHeaders = [];

        if(!(\array_key_exists('content-type', $headers))) {
            $initHeaders['content-type'] = 'text/plain';
        }

        if(!empty(self::$secret)) {
            $initHeaders['x-open-runtimes-secret'] = self::$secret;
            $initHeaders['Authorization'] = 'Basic ' . \base64_encode('opr:' . self::$secret);
        }

        $headers = \array_merge($initHeaders, $headers);
        unset($headers['content-length'], $headers['Content-Length'], $headers['transfer-encoding'], $headers['Transfer-Encoding']);

        $errno = 0;
        $errstr = '';
        $fp = \fsockopen(self::$host, self::$port, $errno, $errstr, $timeout);
        if ($fp === false) {
            return [
                'code' => 0,
                'body' => '',
                'headers' => [],
                'error' => $errstr,
                'errorNumber' => $errno,
            ];
        }

        \stream_set_timeout($fp, $timeout);

        $hostHeader = self::$host . ':' . self::$port;
        $request = \strtoupper($method) . ' ' . $url . " HTTP/1.1\r\n";
        $request .= 'Host: ' . $hostHeader . "\r\n";
        $request .= "Connection: close\r\n";
        $request .= "Transfer-Encoding: chunked\r\n";

        foreach ($headers as $header => $value) {
            $request .= $header . ': ' . $value . "\r\n";
        }

        $request .= "\r\n";

        try {
            self::fwriteAll($fp, $request);

            $offset = 0;
            $length = \strlen($body);
            while ($offset < $length) {
                $chunk = \substr($body, $offset, $chunkSize);
                $offset += \strlen($chunk);
                self::fwriteAll($fp, \sprintf("%x\r\n", \strlen($chunk)));
                self::fwriteAll($fp, $chunk);
                self::fwriteAll($fp, "\r\n");
            }
            self::fwriteAll($fp, "0\r\n\r\n");
        } catch (\Throwable $e) {
            // The server may close the connection once the body limit is
            // reached. Still try to read the status response below.
        }

        $raw = \stream_get_contents($fp);
        \fclose($fp);

        if ($raw === false || $raw === '') {
            return [
                'code' => 0,
                'body' => '',
                'headers' => [],
                'error' => 'Empty HTTP response',
                'errorNumber' => null,
            ];
        }

        return self::parseHttpResponse($raw);
    }

    public static function executeConcurrent(array $requests): array {
        $mh = \curl_multi_init();
        $handles = [];
        $headerBags = [];

        foreach ($requests as $i => $request) {
            $body = $request['body'] ?? '';
            $url = $request['url'] ?? '/';
            $method = $request['method'] ?? 'POST';
            $headers = $request['headers'] ?? [];
            $timeout = $request['timeout'] ?? 5;

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

            $headerBags[$i] = [];
            $ch = \curl_init();
            $optArray = [
                CURLOPT_URL => 'http://' . self::$host . ':' . self::$port . $url,
                CURLOPT_RETURNTRANSFER => true,
                CURLOPT_HEADERFUNCTION => function ($curl, $header) use (&$headerBags, $i) {
                    $len = strlen($header);
                    $header = explode(':', $header, 2);
                    if (count($header) < 2) {
                        return $len;
                    }

                    $key = strtolower(trim($header[0]));
                    $headerBags[$i][$key] = trim($header[1]);
                    return $len;
                },
                CURLOPT_CUSTOMREQUEST => $method,
                CURLOPT_HEADEROPT => \CURLHEADER_UNIFIED,
                CURLOPT_HTTPHEADER => $headersParsed,
                CURLOPT_TIMEOUT => $timeout,
            ];

            if($body !== NULL) {
                $optArray[CURLOPT_POSTFIELDS] = \is_array($body) ? \json_encode($body, JSON_FORCE_OBJECT) : $body;
            }

            \curl_setopt_array($ch, $optArray);
            \curl_multi_add_handle($mh, $ch);
            $handles[$i] = $ch;
        }

        do {
            $status = \curl_multi_exec($mh, $running);
            if ($running) {
                \curl_multi_select($mh, 1.0);
            }
        } while ($running && $status === CURLM_OK);

        $results = [];
        foreach ($handles as $i => $ch) {
            $error = null;
            $errorNumber = null;
            if (\curl_errno($ch)) {
                $errorNumber = \curl_errno($ch);
                $error = \curl_error($ch);
            }

            $results[$i] = [
                'code' => \curl_getinfo($ch, \CURLINFO_HTTP_CODE),
                'body' => \curl_multi_getcontent($ch),
                'headers' => $headerBags[$i] ?? [],
                'error' => $error,
                'errorNumber' => $errorNumber,
            ];

            \curl_multi_remove_handle($mh, $ch);
            \curl_close($ch);
        }

        \curl_multi_close($mh);

        return $results;
    }

    private static function fwriteAll($fp, string $data): void {
        $written = 0;
        $length = \strlen($data);
        while ($written < $length) {
            $n = \fwrite($fp, \substr($data, $written));
            if ($n === false || $n === 0) {
                throw new \RuntimeException('Failed to write HTTP request');
            }
            $written += $n;
        }
    }

    private static function parseHttpResponse(string $raw): array {
        $headerEnd = \strpos($raw, "\r\n\r\n");
        if ($headerEnd === false) {
            return [
                'code' => 0,
                'body' => $raw,
                'headers' => [],
                'error' => 'Invalid HTTP response',
                'errorNumber' => null,
            ];
        }

        $headerBlock = \substr($raw, 0, $headerEnd);
        $body = \substr($raw, $headerEnd + 4);
        $lines = \explode("\r\n", $headerBlock);
        $statusLine = \array_shift($lines) ?? '';
        \preg_match('/\s(\d{3})\b/', $statusLine, $matches);
        $code = isset($matches[1]) ? (int) $matches[1] : 0;

        $responseHeaders = [];
        foreach ($lines as $line) {
            $header = \explode(':', $line, 2);
            if (\count($header) < 2) {
                continue;
            }
            $responseHeaders[\strtolower(\trim($header[0]))] = \trim($header[1]);
        }

        if (\strtolower($responseHeaders['transfer-encoding'] ?? '') === 'chunked') {
            $body = self::decodeChunked($body);
        }

        return [
            'code' => $code,
            'body' => $body,
            'headers' => $responseHeaders,
            'error' => null,
            'errorNumber' => null,
        ];
    }

    private static function decodeChunked(string $data): string {
        $out = '';
        $offset = 0;
        $length = \strlen($data);

        while ($offset < $length) {
            $nl = \strpos($data, "\r\n", $offset);
            if ($nl === false) {
                break;
            }

            $size = \hexdec(\trim(\substr($data, $offset, $nl - $offset)));
            if ($size === 0) {
                break;
            }

            $out .= \substr($data, $nl + 2, $size);
            $offset = $nl + 2 + $size + 2;
        }

        return $out;
    }

    private static function getFile(string $name, int $attempt = 1) {
        if(!\file_exists($name) || \file_exists($name . ".lock")) {
            if($attempt >= 3) {
                return "";
            }

            \sleep(1);
            return self::getFile($name, $attempt + 1);
        }

        return \file_get_contents($name);
    }

    public static function getErrors(string $id) {
        return self::getFile("/tmp/logs/{$id}_errors.log");
    }

    public static function getLogs(string $id) {
        return self::getFile("/tmp/logs/{$id}_logs.log");
    }
}