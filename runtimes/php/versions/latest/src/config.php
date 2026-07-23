<?php

final class Config
{
    public string $secret;
    public array $headers;
    public string $entrypoint;
    public string $env;

    public function __construct()
    {
        $this->secret = \getenv('OPEN_RUNTIMES_SECRET') ?: '';
        $this->headers = \json_decode(\getenv('OPEN_RUNTIMES_HEADERS') ?: '{}', true) ?: [];
        $this->entrypoint = \getenv('OPEN_RUNTIMES_ENTRYPOINT') ?: '';
        $this->env = \getenv('OPEN_RUNTIMES_ENV') ?: '';
    }
}
