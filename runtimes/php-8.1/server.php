<?php
$server = new Swoole\HTTP\Server("0.0.0.0", 3000);

const USER_CODE_PATH = '/usr/code-start';

class Response {
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

class Request {
    public string $rawBody = '';
    public mixed $body = '';
    public array $headers = [];
    public string $method = '';
    public string $url = '';
}

class Context {
    public Request $req;
    public Response $res;

    public array $_logs = [];
    public array $_errors = [];

    function __construct() {
        $this->req = new Request();
        $this->res = new Response();
    }

    // TODO: Support many params
    function log(mixed $message) {
        $this->_logs[] = \strval($message);
    }

    function error(mixed $message) {
        $this->_errors[] = \strval($message);
    }
}

$userFunction = null;

$server->on("Request", function($req, $res) use(&$userFunction) {
    if (empty($req->header['x-open-runtimes-secret']) || $req->header['x-open-runtimes-secret'] !== getenv('OPEN_RUNTIMES_SECRET')) {
        $res->status(500);
        $res->end('Unauthorized. Provide correct "x-open-runtimes-secret" header.');
        return;
    }

    $context = new Context();

    $context->req->rawBody = $req->getContent();
    $context->req->body = $context->req->rawBody;
    $context->req->method = $req->getMethod();
    $context->req->url = $req->header['x-open-runtimes-original-url'] ?? '';
    $context->req->headers = [];

    $contentType = $req->header['content-type'] ?? 'text/plain';
    if(\str_contains($contentType, 'application/json')) {
        $context->req->body = json_decode($context->req->rawBody, true);
    }

    foreach ($req->header as $header => $value) {
        if(!(\str_starts_with(\strtolower($header), 'x-open-runtimes-'))) {
            $context->req->headers[\strtolower($header)] = $value;
        }
    }

    $customstd = null;

    $output = null;
    try {
        if($userFunction === null) {
            $userFunction = include(USER_CODE_PATH . '/' . getenv('OPEN_RUNTIMES_ENTRYPOINT'));
        }

        if (!is_callable($userFunction)) {
            throw new Exception('User function is not valid.');
        }

        ob_start();
        $output = $userFunction($context);
        $customstd = ob_get_clean();
    } catch (\Throwable $e) {
        $context->error($e->getMessage()."\n".$e->getTraceAsString());
        $output = $context->res->send('', 500, []);
    }

    if($output == null) {
        $context->error('Return statement missing. return $context->res->empty() if no response is expected.');
        $output = $context->res->send('', 500, []);
    }

    $output['body'] ??= '';
    $output['statusCode'] ??= 200;
    $output['headers'] ??= [];

    foreach ($output['headers'] as $header => $value) {
        if(!(\str_starts_with(\strtolower($header), 'x-open-runtimes-'))) {
            $res->header(\strtolower($header), $value);
        }
    }

    if(!empty($customstd)) {
        $context->log('Unsupported log noticed. Use $context->log() or $context->error() for logging.');
    }

    $res->header('x-open-runtimes-logs', \urlencode(\implode('\n', $context->_logs)));
    $res->header('x-open-runtimes-errors', \urlencode(\implode('\n', $context->_errors)));

    $res->status($output['statusCode']);
    $res->end($output['body']);
});

$server->start();