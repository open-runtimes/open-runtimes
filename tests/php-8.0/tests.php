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
    'env' - object with environment variables
  '$res' variable has:
    'send(text, status)' - function to return text response. Status code defaults to 200
    'json(obj, status)' - function to return JSON response. Status code defaults to 200
  If an error is thrown, a response with code 500 will be returned.
*/

return function($req, $res) use ($client) {
    $payload = \json_decode($req['payload'] === '' ? '{}' : $req['payload'], true);

    $response = $client->request('GET', '/todos/' . ($payload['id'] ?? 1));
    $todo = \json_decode($response->getBody()->getContents(), true);

    $res->json([
        'isTest' => true,
        'message' => 'Hello Open Runtimes 👋',
        'header' => $req['headers']['x-test-header'],
        'env' => $req['env']['test-env'],
        'todo' => $todo
    ]);
};