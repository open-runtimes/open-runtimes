<?php

require 'vendor-server/autoload.php';
require_once 'types.php';
require_once 'logger.php';

Swoole\Runtime::enableCoroutine($flags = SWOOLE_HOOK_ALL);
$payloadSize = 20 * (1024 * 1024);
$server = new Swoole\HTTP\Server("0.0.0.0", 3000);
$server->set([
    'package_max_length' => $payloadSize,
    'buffer_output_size' => $payloadSize,
]);

const USER_CODE_PATH = '/usr/local/server/src/function';

$userFunction = null;

$action = function (Logger $logger, mixed $req, mixed $res) use (&$userFunction) {
    $requestHeaders = $req->header;

    $cookieHeaders = [];
    foreach (($req->cookie ?? []) as $key => $value) {
        $cookieHeaders[] = "{$key}={$value}";
    }

    if (!empty($cookieHeaders)) {
        $requestHeaders['cookie'] = \implode('; ', $cookieHeaders);
    }

    $timeout = $requestHeaders['x-open-runtimes-timeout'] ?? '';
    $safeTimeout = null;

    if ($timeout) {
        if (!\is_numeric($timeout) || \intval($timeout) === 0) {
            $res->status(500);
            $res->end('Header "x-open-runtimes-timeout" must be an integer greater than 0.');
            return;
        }

        $safeTimeout = \intval($timeout);
    }

    if ((getenv('OPEN_RUNTIMES_SECRET') ?? '') != "" && ($requestHeaders['x-open-runtimes-secret'] ?? '') !== getenv('OPEN_RUNTIMES_SECRET')) {
        $res->status(500);
        $res->end('Unauthorized. Provide correct "x-open-runtimes-secret" header.');
        return;
    }

    $path = $req->server['path_info'];
    $scheme = ($requestHeaders['x-forwarded-proto'] ?? 'http');
    $defaultPort = $scheme === 'https' ? '443' : '80';
    $query = [];

    $hostHeader = ($requestHeaders['host'] ?? '');
    if (\str_contains($hostHeader, ':')) {
        $pair = \explode(':', $hostHeader);
        $host = $pair[0];
        $port = \intval($pair[1]);
    } else {
        $host = $hostHeader;
        $port = \intval($defaultPort);
    }

    $queryString = $req->server['query_string'] ?? '';
    foreach (\explode('&', $queryString) as $param) {
        $pair = \explode('=', $param, 2);
        if (!empty($pair[0])) {
            $query[$pair[0]] = $pair[1] ?? '';
        }
    }

    $url = $scheme . '://' . $host;

    if ($port !== \intval($defaultPort)) {
        $url .= ':' . \strval($port);
    }

    $url .= $path;

    if (!empty($queryString)) {
        $url .= '?' . $queryString;
    }

    $context = new RuntimeContext($logger);
    $context->req->bodyBinary = $req->getContent();
    $context->req->method = $req->getMethod();
    $context->req->url = $url;
    $context->req->path = $path;
    $context->req->port = $port;
    $context->req->host = $host;
    $context->req->scheme = $scheme;
    $context->req->query = $query;
    $context->req->queryString = $queryString;
    $context->req->headers = [];

    foreach ($requestHeaders as $header => $value) {
        if (!(\str_starts_with(\strtolower($header), 'x-open-runtimes-'))) {
            $context->req->headers[\strtolower($header)] = $value;
        }
    }

    $enforcedHeaders = json_decode(getenv('OPEN_RUNTIMES_HEADERS') ?? '{}', true);
    foreach ($enforcedHeaders as $key => $value) {
        $context->req->headers[\strtolower($key)] = \strval($value);
    }

    $context->req->headers = array_change_key_case($context->req->headers);

    $output = null;

    $execute = function () use ($userFunction, &$output, $context, $logger) {
        if ($userFunction === null) {
            $userFunction = include(USER_CODE_PATH . '/' . getenv('OPEN_RUNTIMES_ENTRYPOINT'));
        }

        if (!is_callable($userFunction)) {
            throw new Exception('User function is not valid.');
        }

        $logger->overrideNativeLogs();
        $output = $userFunction($context);
        $logger->revertNativeLogs();
    };

    try {
        if ($safeTimeout !== null) {
            $executed = false;
            Swoole\Coroutine\batch([
                function () use ($execute, &$executed) {
                    \call_user_func($execute);
                    $executed = true;
                }
            ], $safeTimeout);

            if (!$executed) {
                $context->error('Execution timed out.');
                $output = $context->res->text('', 500);
            }
        } else {
            \call_user_func($execute);
        }
    } catch (\Throwable $e) {
        $context->error($e->getMessage()."\n".$e->getTraceAsString());
        $context->error('At ' . $e->getFile() . ':' . $e->getLine());
        $output = $context->res->text('', 500);
    }

    if ($output == null) {
        $context->error('Return statement missing. return $context->res->empty() if no response is expected.');
        $output = $context->res->text('', 500);
    }

    $output['body'] ??= '';
    $output['statusCode'] ??= 200;
    $output['headers'] ??= [];

    $headers = \array_change_key_case($output['headers']);

    if (!empty($headers['content-type'])) {
        $headers['content-type'] = \strtolower($headers['content-type']);
    }

    if (!isset($headers['content-type'])) {
        $headers['content-type'] = 'text/plain; charset=utf-8';
    }

    if (!\str_starts_with($headers['content-type'], 'multipart/') && !\str_contains($headers['content-type'], 'charset=')) {
        $headers['content-type'] .= '; charset=utf-8';
    }

    foreach ($headers as $header => $value) {
        if (!(\str_starts_with($header, 'x-open-runtimes-'))) {
            $res->header($header, $value);
        }
    }

    $res->header('x-open-runtimes-log-id', $logger->id);
    $logger->end();

    $res->status($output['statusCode']);
    $res->end($output['body']);
};

$server->on("Request", function ($req, $res) use ($action) {
    $logger = new Logger($req->header['x-open-runtimes-logging'] ?? '', $req->header['x-open-runtimes-log-id'] ?? '');

    try {
        $action($logger, $req, $res);
    } catch (\Throwable $e) {
        $message = $e->getMessage() . "\n";
        $message .= $e->getTraceAsString() . "\n";
        $message .= 'In ' . $e->getFile() . ':' . $e->getLine() . "\n";

        $logger->write($message, Logger::TYPE_ERROR);

        $res->header('x-open-runtimes-log-id', $logger->id);
        $logger->end();

        $res->status(500);
        $res->end('');
    }
});

$server->on("Start", function () {
    echo("HTTP server successfully started!\n");
});

$server->start();
