<?php 

require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client([
    'base_uri' => 'https://jsonplaceholder.typicode.com'
]);

/*
  '$req' variable has:
    'headers' - object with request headers
    'payload' - object with request body data
    'variables' - object with function variables
  '$res' variable has:
    'send(text, status)' - function to return text response. Status code defaults to 200
    'json(obj, status)' - function to return JSON response. Status code defaults to 200
  If an error is thrown, a response with code 500 will be returned.
*/

return function($req, $res) use ($client) {
    $payload = \json_decode($req['payload'], true);

    $response = $client->request('GET', '/todos/' . ($payload['id'] ?? 1));
    $todo = \json_decode($response->getBody()->getContents(), true);

    echo "log1\n";
    print("{hello: world}\n");
    print("[hello, world]");
    
    $res->json([
        'message' => 'Hello Open Runtimes 👋',
        'todo' => $todo
    ]);
};