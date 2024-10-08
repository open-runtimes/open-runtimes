<?php

class RuntimeResponse
{
    public function binary(string $binary, int $statusCode = 200, array $headers = []): array
    {
        return [
            'body' => $binary,
            'statusCode' => $statusCode,
            'headers' => $headers,
        ];
    }

    public function send(string $body, int $statusCode = 200, array $headers = []): array
    {
        return $this->text(\strval($body), $statusCode, $headers);
    }


    public function text(string $body, int $statusCode = 200, array $headers = []): array
    {
        return $this->binary($body, $statusCode, $headers);
    }

    public function json(array $obj, int $statusCode = 200, array $headers = []): array
    {
        $headers['content-type'] = 'application/json';
        return $this->text(\json_encode($obj, JSON_FORCE_OBJECT), $statusCode, $headers);
    }

    public function empty(): array
    {
        return $this->text('', 204, []);
    }

    public function redirect(string $url, int $statusCode = 301, array $headers = []): array
    {
        $headers['location'] = $url;
        return $this->text('', $statusCode, $headers);
    }
}

class RuntimeRequest
{
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
            'bodyText' => $this->getBodyText(),
            default => ''
        };
    }

    private function getBody()
    {
        $contentType = strtolower($this->headers['content-type'] ?? 'text/plain');

        if (\str_starts_with($contentType, 'application/json')) {
            if (!empty($this->bodyBinary)) {
                return $this->getBodyJson();
            } else {
                return [];
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

class RuntimeContext
{
    public RuntimeRequest $req;
    public RuntimeResponse $res;
    private Logger $logger;

    public function __construct(Logger $logger)
    {
        $this->req = new RuntimeRequest();
        $this->res = new RuntimeResponse();
        $this->logger = $logger;
    }

    public function log(mixed ...$messages): void
    {
        $this->logger->write($messages, Logger::TYPE_LOG);
    }

    public function error(mixed ...$messages): void
    {
        $this->logger->write($messages, Logger::TYPE_ERROR);
    }
}
