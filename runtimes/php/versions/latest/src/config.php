<?php

final class Config
{
    public string $secret;
    public array $headers;
    public string $entrypoint;
    public string $env;
    public string $logsDirectory;
    public bool $inlineLogs;

    public function __construct()
    {
        $this->secret = \getenv('OPEN_RUNTIMES_SECRET') ?: '';
        $this->headers = \json_decode(\getenv('OPEN_RUNTIMES_HEADERS') ?: '{}', true) ?: [];
        $this->entrypoint = \getenv('OPEN_RUNTIMES_ENTRYPOINT') ?: '';
        $this->env = \getenv('OPEN_RUNTIMES_ENV') ?: '';
        // Inline logs travel on the response, so no log files are written
        $this->inlineLogs = \getenv('OPEN_RUNTIMES_LOGGING') === 'inline';
        $logsDirectory = \getenv('OPEN_RUNTIMES_LOGS_DIRECTORY');
        $this->logsDirectory = $this->inlineLogs ? '' : ($logsDirectory === false ? '/mnt/logs' : $logsDirectory);
    }
}
