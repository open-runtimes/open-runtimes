<?php
$server = new Swoole\HTTP\Server("0.0.0.0", 3000);

const USER_CODE_PATH = '/usr/code-start';

function join_paths() {
    $paths = array();

    foreach (func_get_args() as $arg) {
        if ($arg !== '') { $paths[] = $arg; }
    }

    return preg_replace('#/+#','/',join('/', $paths));
}

class Response {
    private mixed $response;
    private int $status = 200;
    private array $headers = [];

    function getStatus(): int {
        return $this->status;
    }

    function getResponse(): mixed  {
        return $this->response;
    }

    function getHeaders(): array {
        return $this->headers;
    }

    function send($text, $status = 200) {
        $this->response = $text;
        $this->status = $status;
    }

    function json($json, $status = 200) {
        $this->status = $status;
        $this->response = $json;
        $this->headers['Content-Type'] = 'application/json';
    }
}

$userFunction = null;

$server->on("Request", function($req, $res) use(&$userFunction) {
    $body =  json_decode($req->getContent(), true);
    $body['payload'] = \is_string($body['payload']) ? $body['payload'] : \json_encode($body['payload'], JSON_FORCE_OBJECT);

    // Get internal_challenge header
    $internal_challenge = $req->header['x-internal-challenge'];

    if (empty($internal_challenge)) {
        $res->status(500);
        $res->end(json_encode(['stderr' => 'Unauthorized']));
        return;
    }

    $key = getenv('INTERNAL_RUNTIME_KEY');

    if ($key != $internal_challenge) {
        $res->status(500);
        $res->end('Unauthorized');
        return;
    }

    $request = [
        'env' => $body['env'] ?? [],
        'headers' => $body['headers'] ?? [],
        'payload' => $body['payload'] ?? ''
    ];

    $response = new Response($res);

    try {
        if($userFunction === null) {
            $userFunction = include(USER_CODE_PATH . '/' . getenv('INTERNAL_RUNTIME_ENTRYPOINT'));
        }

        if (!is_callable($userFunction)) {
            return throw new Exception('Function not valid');
        }
        ob_start();
        $userFunction($request, $response);
        $stdout = ob_get_clean();

        $res->status($response->getStatus());
        $res->header = $response->getHeaders();
        $res->end(json_encode(['response' => $response->getResponse(), 'stdout' => $stdout], JSON_UNESCAPED_UNICODE|JSON_UNESCAPED_SLASHES));
        $stdout = '';
    } catch (\Throwable $e) {
        $res->status(500);
        return $res->end(json_encode(['stderr' => $e->getMessage()."\r\n".$e->getTraceAsString()], JSON_UNESCAPED_UNICODE|JSON_UNESCAPED_SLASHES));
    }
});

$server->start();