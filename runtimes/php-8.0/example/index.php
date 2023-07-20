<?php 

require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client([
    'base_uri' => 'https://jsonplaceholder.typicode.com'
]);

return function($context) use ($client) { 
    $payload = $context->req->body;

    $response = $client->request('GET', '/todos/' . ($payload['id'] ?? 1));
    $todo = \json_decode($response->getBody()->getContents(), true);

    return $context->res->json([
        'message' => 'Hello Open Runtimes 👋',
        'todo' => $todo
    ]);
};