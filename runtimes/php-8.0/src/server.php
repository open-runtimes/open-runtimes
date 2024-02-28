<?php

require 'vendor-server/autoload.php';

Swoole\Runtime::enableCoroutine($flags = SWOOLE_HOOK_ALL);

$server = new Swoole\HTTP\Server("0.0.0.0", 3000);

const USER_CODE_PATH = '/usr/local/server/src/function';

class RuntimeResponse {
    function send(string $body, int $statusCode = 200, array $headers = []): array {
        return [
            'body' => $body,
            'statusCode' => $statusCode,
            'headers' => $headers,
        ];
    }

    function json(array $obj, int $statusCode = 200, array $headers = []) {
        $headers['content-type'] = 'application/json';
        return $this->send(\json_encode($obj), $statusCode, $headers);
    }

    function empty() {
        return $this->send('', 204, []);
    }

    function redirect(string $url, int $statusCode = 301, array $headers = []) {
        $headers['location'] = $url;
        return $this->send('', $statusCode, $headers);
    }
}

class RuntimeRequest {
    public string $bodyRaw = '';
    public mixed $body = '';
    public array $headers = [];
    public string $method = '';
    public string $url = '';
    public string $path = '';
    public int $port = 80;
    public string $host = '';
    public string $scheme = '';
    public string $queryString = '';
    public array $query = [];
}

class RuntimeContext {
    public RuntimeRequest $req;
    public RuntimeResponse $res;

    public array $logs = [];
    public array $errors = [];

    function __construct() {
        $this->req = new RuntimeRequest();
        $this->res = new RuntimeResponse();
    }

    function log(mixed $message): void
    {
        if(\is_array($message) || \is_object($message)) {
            $this->logs[] = \json_encode($message);
        } else {
            $this->logs[] = \strval($message);
        }
    }

    function error(mixed $message): void
    {
        if(\is_array($message) || \is_object($message)) {
            $this->errors[] = \json_encode($message);
        } else {
            $this->errors[] = \strval($message);
        }
    }
}

$userFunction = null;

$action = function($req, $res) use (&$userFunction) {
    $requestHeaders = $req->header;

    $cookieHeaders = [];
    foreach ($req->cookie as $key => $value) {
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

    if (($requestHeaders['x-open-runtimes-secret'] ?? '') === '' || ($requestHeaders['x-open-runtimes-secret'] ?? '') !== (getenv('OPEN_RUNTIMES_SECRET') ?? '')) {
        $res->status(500);
        $res->end('Unauthorized. Provide correct "x-open-runtimes-secret" header.');
        return;
    }

    $path = $req->server['path_info'];
    $scheme = ($requestHeaders['x-forwarded-proto'] ?? 'http');
    $defaultPort = $scheme === 'https' ? '443' : '80';
    $query = [];

    $hostHeader = ($requestHeaders['host'] ?? '');
    if(\str_contains($hostHeader, ':')) {
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
        if(!empty($pair[0])) {
            $query[$pair[0]] = $pair[1] ?? '';
        }
    }

    $url = $scheme . '://' . $host;

    if($port !== \intval($defaultPort)) {
        $url .= ':' . \strval($port);
    }

    $url .= $path;

    if(!empty($queryString)) {
        $url .= '?' . $queryString;
    }

    $context = new RuntimeContext();

    $context->req->bodyRaw = $req->getContent();
    $context->req->body = $context->req->bodyRaw;
    $context->req->method = $req->getMethod();
    $context->req->url = $url;
    $context->req->path = $path;
    $context->req->port = $port;
    $context->req->host = $host;
    $context->req->scheme = $scheme;
    $context->req->query = $query;
    $context->req->queryString = $queryString;
    $context->req->headers = [];

    $contentType = $requestHeaders['content-type'] ?? 'text/plain';
    if(\str_contains($contentType, 'application/json')) {
        if(!empty($context->req->bodyRaw)) {
            $context->req->body = json_decode($context->req->bodyRaw, true);

            if($context->req->body === null) {
                throw new \Exception('Invalid JSON body.');
            }
        } else {
            $context->req->body = [];
        }
    }

    foreach ($requestHeaders as $header => $value) {
        if(!(\str_starts_with(\strtolower($header), 'x-open-runtimes-'))) {
            $context->req->headers[\strtolower($header)] = $value;
        }
    }

    $customStd = null;
    $output = null;

    $execute = function() use ($userFunction, &$output, &$customStd, $context) {
        if($userFunction === null) {
            $userFunction = include(USER_CODE_PATH . '/' . getenv('OPEN_RUNTIMES_ENTRYPOINT'));
        }

        if (!is_callable($userFunction)) {
            throw new Exception('User function is not valid.');
        }

        ob_start();
        $output = $userFunction($context);
        $customStd = ob_get_clean();
    };

    try {
        if($safeTimeout !== null) {
            $executed = false;
            Swoole\Coroutine\batch([
                function() use ($execute, &$executed) {
                    \call_user_func($execute);
                    $executed = true;
                }
            ], $safeTimeout);

            if(!$executed) {
                $context->error('Execution timed out.');
                $output = $context->res->send('', 500);
            }
        } else {
            \call_user_func($execute);
        }
    } catch (\Throwable $e) {
        $context->error($e->getMessage()."\n".$e->getTraceAsString());
        $context->error('At ' . $e->getFile() . ':' . $e->getLine());
        $output = $context->res->send('', 500);
    }

    if($output == null) {
        $context->error('Return statement missing. return $context->res->empty() if no response is expected.');
        $output = $context->res->send('', 500);
    }

    $output['body'] ??= '';
    $output['statusCode'] ??= 200;
    $output['headers'] ??= [];

    $headers = \array_change_key_case($output['headers']);

    if (!isset($headers['content-type'])) {
        $headers['content-type'] = 'text/plain; charset=utf-8';
    }

    if (!\str_starts_with($headers['content-type'], 'multipart/') && !\str_contains($headers['content-type'], 'charset=')) {
        $headers['content-type'] .= '; charset=utf-8';
    }

    foreach ($headers as $header => $value) {
        if(!(\str_starts_with($header, 'x-open-runtimes-'))) {
            $res->header($header, $value);
        }
    }
 
    if(!empty($customStd)) {
        $context->log("");
        $context->log("----------------------------------------------------------------------------");
        $context->log("Unsupported logs detected. Use \$context->log() or \$context->error() for logging.");
        $context->log("----------------------------------------------------------------------------");
        $context->log($customStd);
        $context->log("----------------------------------------------------------------------------");
    }

    $res->header('x-open-runtimes-logs', \urlencode(\implode('\n', $context->logs)));
    $res->header('x-open-runtimes-errors', \urlencode(\implode('\n', $context->errors)));

    $res->status($output['statusCode']);
    $res->end($output['body']);
};

$server->on("Request", function($req, $res) use($action) {
    try {
        $action($req, $res);
    } catch (\Throwable $e) {
        $logs = [];
        $errors = [
            $e->getMessage()."\n".$e->getTraceAsString(),
            'At ' . $e->getFile() . ':' . $e->getLine()
        ];

        $res->header('x-open-runtimes-logs', \urlencode(\implode('\n', $logs)));
        $res->header('x-open-runtimes-errors', \urlencode(\implode('\n', $errors)));
    
        $res->status(500);
        $res->end('');
    }
});

$server->start();