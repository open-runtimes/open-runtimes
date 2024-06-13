<?php

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
    private Logger $logger;

    function __construct(Logger $logger) {
        $this->req = new RuntimeRequest();
        $this->res = new RuntimeResponse();
        $this->logger = $logger;
    }

    function log(mixed $message): void
    {
        $this->logger->write($message, Logger::TYPE_LOG);
    }

    function error(mixed $message): void
    {
        $this->logger->write($message, Logger::TYPE_ERROR);
    }
}