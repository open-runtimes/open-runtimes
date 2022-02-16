<?php
$server = new Swoole\HTTP\Server("0.0.0.0", 3000);

const DEFAULT_PATH = '/usr/code';
const DEFAULT_FILE = 'index.php';

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


$server->on("Request", function($req, $res) {
    $body =  json_decode($req->getContent(), true);

    // Get internal_challenge header
    $internal_challenge = $req->header['x-internal-challenge'];

    if (empty($internal_challenge)) {
        $res->status(401);
        $res->end(json_encode([
            'code' => 401,
            'message' => 'Unauthorized',
        ]));
        return;
    }

    $key = getenv('INTERNAL_RUNTIME_KEY');

    if ($key != $internal_challenge) {
        $res->status(401);
        $res->end(json_encode([
            'code' => 401,
            'message' => 'Unauthorized',
        ]));
        return;
    }

    $request = new stdClass();

    $request->env = $body['env'] ?? [];
    $request->headers = $body['headers'] ?? [];
    $request->payload = $body['payload'] ?? '{}';

    $response = new Response($res);

    try {
        $userFunction = include(join_paths($body['path'] ?? DEFAULT_PATH, $body['file'] ?? DEFAULT_FILE));

        if (!is_callable($userFunction)) {
            return throw new Exception('Function not valid');
        }
        $userFunction($request, $response);
    } catch (\Throwable $e) {
        $res->status(500);
        return $res->end(json_encode([
            'code' => 500,
            'message' => $e->getMessage()."\r\n".$e->getTraceAsString(),
        ]));
    }
});

$server->start();