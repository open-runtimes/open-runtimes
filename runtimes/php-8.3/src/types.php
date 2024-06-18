<?php

class RuntimeResponse {
    private $chunkHeadersSent = false;
    private $res;

    public function __construct(mixed $res)
    {
        $this->res = $res;
    }

    public function getChunkStatus()
    {
        return $this->chunkHeadersSent;
    }

    function binary(array $bytes, int $statusCode = 200, array $headers = []): array {
        return [
            'body' => $bytes,
            'statusCode' => $statusCode,
            'headers' => $headers,
            'chunked' => false,
        ];
    }

    function send(string $body, int $statusCode = 200, array $headers = []): array {
        return $this->text(\strval($body),$statusCode, $headers);
    }


    function text(string $body, int $statusCode = 200, array $headers = []): array {
        $bin = \unpack("C*", $body);
        return $this->binary($bin, $statusCode, $headers);
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

    /**
     * @throws Exception
     */
    function start($statusCode = 200, array $headers = []) {
        if(!$this->chunkHeadersSent) {
            $this->chunkHeadersSent = true;

            $headers['cache-control'] =  $headers['cache-control'] ?? 'no-store';
            $headers['content-type'] =  $headers['content-type'] ?? 'text/event-stream';
            $headers['connection'] =  $headers['connection'] ?? 'keep-alive';
            $headers['transfer-encoding'] =  $headers['transfer-encoding'] ?? 'chunked';

            foreach ($headers as $key => $value) {
                $this->res->header($key, $value);
            }
            $this->res->status($statusCode);
        }else{
            throw new Exception('You can only call $res->start() once');
        }
    }

    function writeText($body){
        $this->writeBinary(\unpack("C*", $body));
    }
    function writeJson($body){
        $this->writeText(json_encode($body));
    }
    function writeBinary($body){
        if(!$this->chunkHeadersSent){
            throw new Exception('You must call $res->start() to start a chunk response.');
        }

        $this->res->write(pack("C*",...$body));
    }

    function end(array $headers = []): array {
        if(!$this->chunkHeadersSent){
            throw new Exception('You must call $res->start() to start a chunk response.');
        }

        return [
            'body' => '',
            'statusCode' => 0,
            'headers' => $headers,
            'chunked' => true,
        ];
    }
}

class RuntimeRequest {
    public array $bodyBinary = [];
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

        if(\str_contains($contentType, 'application/json')) {
            return $this->getBodyJson();
        }

        $binaryTypes = ["application/", "audio/", "font/", "image/", "video/"];
        foreach ($binaryTypes as $type) {
            if(\str_contains($contentType, $type)) {
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
        return pack('C*', ...$this->bodyBinary);
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

    function __construct(Logger $logger, mixed $res) {
        $this->req = new RuntimeRequest();
        $this->res = new RuntimeResponse($res);
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
