<?php

class RuntimeResponse {
    function binary(string $binary, int $statusCode = 200, array $headers = []): array {
        return [
            'body' => $binary,
            'statusCode' => $statusCode,
            'headers' => $headers,
        ];
    }

    function send(string $body, int $statusCode = 200, array $headers = []): array {
        return $this->text(\strval($body),$statusCode, $headers);
    }


    function text(string $body, int $statusCode = 200, array $headers = []): array {
        return $this->binary($body, $statusCode, $headers);
    }

    function json(array $obj, int $statusCode = 200, array $headers = []): array {
        $headers['content-type'] = 'application/json';
        return $this->text(\json_encode($obj, JSON_FORCE_OBJECT), $statusCode, $headers);
    }

    function empty(): array {
        return $this->text('', 204, []);
    }

    function redirect(string $url, int $statusCode = 301, array $headers = []): array {
        $headers['location'] = $url;
        return $this->text('', $statusCode, $headers);
    }
}

class RuntimeRequest {
    public string $bodyBinary = '';
    public array $headers = [];
    public string $method = '';
    public string $url = '';
    public string $path = '';
    public int $port = 80;
    public string $host = '';
    public string $scheme = '';
    public string $queryString = '';
    public array $query = [];

    public function __get(string $name)
    {
        return match ($name) {
            'body' => $this->getBody(),
            'bodyRaw' => $this->getRawBody(),
            'bodyJson' => $this->getBodyJson(),
            default => ''
        };
    }

    private function getBody()
    {
        $contentType = strtolower($this->headers['content-type'] ?? 'text/plain');

        if(\str_starts_with($contentType, 'application/json')) {
            if(!empty($this->bodyBinary)) {
                return $this->getBodyJson();
            } else {
                return [];
            }
        }

        $binaryTypes = ["application/", "audio/", "font/", "image/", "video/"];
        foreach ($binaryTypes as $type) {
            if(\str_starts_with($contentType, $type)) {
                return  $this->bodyBinary;
            }
        }

        return $this->getBodyText();
    }
    private function getRawBody(): string
    {
        return $this->getBodyText();
    }
    private function getBodyText(): string
    {
        return $this->bodyBinary;
    }
    private function getBodyJson(): array
    {
        return \json_decode($this->getBodyText(), true);
    }
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
