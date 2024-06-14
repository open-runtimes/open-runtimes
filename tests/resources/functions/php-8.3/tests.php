<?php

require 'vendor/autoload.php';

use GuzzleHttp\Client;

$client = new Client([
    'base_uri' => 'https://jsonplaceholder.typicode.com'
]);

return function($context) use ($client) {
    $action = $context->req->headers['x-action'];

    switch ($action) {
        case 'plaintextResponse':
            return $context->res->text('Hello World 👋');
        case 'jsonResponse':
            return $context->res->json([ 'json' => true, 'message' => 'Developers are awesome.' ]);
        case 'customCharsetResponse':
            return $context->res->text('ÅÆ', 200, [ 'content-type' => 'text/plain; charset=iso-8859-1' ]);
        case 'uppercaseCharsetResponse':
            return $context->res->text('ÅÆ', 200, [ 'content-type' => 'TEXT/PLAIN' ]);
        case 'multipartResponse':
            return $context->res->text("--12345
Content-Disposition: form-data; name=\"partOne\"

Why just have one part?
--12345
Content-Disposition: form-data; name=\"partTwo\"

When you can have two!
--12345--", 200, [ 'content-type' => 'multipart/form-data; boundary=12345' ]);
        case 'redirectResponse':
            return $context->res->redirect('https://github.com/');
        case 'emptyResponse':
            return $context->res->empty();
        case 'noResponse':
            $context->res->text('This should be ignored, as it is not returned.');
            break;
        case 'doubleResponse':
            $context->res->text('This should be ignored.');
            return $context->res->text('This should be returned.');
        case 'headersResponse':
            return $context->res->text('OK', 200, [
                'first-header' => 'first-value',
                'second-header' => $context->req->headers['x-open-runtimes-custom-in-header'] ?? 'missing',
                'cookie' => $context->req->headers['cookie'] ?? 'missing',
                'x-open-runtimes-custom-out-header' => 'third-value'
            ]);
        case 'statusResponse':
            return $context->res->text('FAIL', 404);
        case 'requestMethod':
            return $context->res->text($context->req->method);
        case 'requestUrl':
            return $context->res->json([
                'url' => $context->req->url,
                'port' => $context->req->port,
                'path' => $context->req->path,
                'query' => $context->req->query,
                'queryString' => $context->req->queryString,
                'scheme' => $context->req->scheme,
                'host' => $context->req->host,
            ]);
            return $context->res->text($context->req->url);
        case 'requestHeaders':
            return $context->res->json($context->req->headers);
        case 'requestBodyText':
            return $context->res->text($context->req->body);
        case 'requestBodyJson':
            return $context->res->json($context->req->bodyJson);
        case 'requestBodyBinary':
            return $context->res->binary($context->req->bodyBinary);
        case 'requestBodyTextAuto':
            return $context->res->text($context->req->body);
        case 'requestBodyJsonAuto':
            return $context->res->json($context->req->body);
        case 'requestBodyBinaryAuto':
            return $context->res->binary($context->req->body);
        case 'binaryResponse1':
            $bytes = [0, 10, 255];
            return $context->res->binary($bytes); // List<int>
        case 'binaryResponse2':
            $bytes = [0, 20, 255];
            return $context->res->binary($bytes); // Just a filler
        case 'binaryResponse3':
            $bytes = [0, 30, 255];
            return $context->res->binary($bytes); // Just a filler
        case 'binaryResponse4':
            $bytes = [0, 40, 255];
            return $context->res->binary($bytes); // Just a filler
        case 'binaryResponse5':
            $bytes = [0, 50, 255];
            return $context->res->binary($bytes); // Just a filler
        case 'envVars':
            $var = getenv('CUSTOM_ENV_VAR');
            $emptyVar = getenv('NOT_DEFINED_VAR');
            return $context->res->json([
                'var' => $var === false ? null : $var,
                'emptyVar' => $emptyVar === false ? null : $emptyVar,
            ]);
        case 'logs':
            \var_dump('Native log');
            $context->log('Debug log');
            $context->error('Error log');

            $context->log("Log+With+Plus+Symbol");

            $context->log(42);
            $context->log(4.2);
            $context->log('true'); // true logs as 1

            $context->log([ 'objectKey' => 'objectValue' ]);
            $context->log([ 'arrayValue' ]);

            return $context->res->text('');
        case 'library':
            $response = $client->request('GET', '/todos/' . $context->req->bodyRaw);
            $todo = \json_decode($response->getBody()->getContents(), true);
            return $context->res->json([ 'todo' => $todo ]);
        case 'timeout':
            $context->log('Timeout start.');
            \sleep(3);

            $context->log('Timeout end.');
            return $context->res->text('Successful response.');
        case 'deprecatedMethods':
            return $context->res->send($context->req->bodyRaw);
        default:
            throw new Exception('Unknown action');
    }
};
