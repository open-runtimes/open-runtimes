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
    function __construct($res) {
        $this->res = $res;
    }

    function send($text, $status = 200) {
        $this->res->status($status);
        $this->res->end($text);
    }

    function json($json, $status = 200) {
        $this->res->status($status);
        $this->res->headers['Content-Type'] = 'application/json';
        $this->res->end(json_encode($json, JSON_UNESCAPED_UNICODE|JSON_UNESCAPED_SLASHES));
    }
}

$userFunction = null;

$server->on("Request", function($req, $res) use(&$userFunction) {
    $body =  json_decode($req->getContent(), true);
    $body['payload'] = \is_string($body['payload']) ? $body['payload'] : \json_encode($body['payload'], JSON_FORCE_OBJECT);

    // Get internal_challenge header
    $internal_challenge = $req->header['x-internal-challenge'];

    if (empty($internal_challenge)) {
        $res->status(401);
        $res->end('Unauthorized');
        return;
    }

    $key = getenv('INTERNAL_RUNTIME_KEY');

    if ($key != $internal_challenge) {
        $res->status(401);
        $res->end('Unauthorized');
        return;
    }

    $request = [
        'env' => $body['env'] ?? [],
        'headers' => $body['headers'] ?? [],
        'payload' => $body['payload'] ?? '{}'
    ];

    $response = new Response($res);

    try {
        if($userFunction === null) {
            $userFunction = include(USER_CODE_PATH . '/' . getenv('INTERNAL_RUNTIME_ENTRYPOINT'));
        }

        if (!is_callable($userFunction)) {
            return throw new Exception('Function not valid');
        }
        $userFunction($request, $response);
    } catch (\Throwable $e) {
        $res->status(500);
        return $res->end($e->getMessage()."\r\n".$e->getTraceAsString());
    }
});

$server->start();