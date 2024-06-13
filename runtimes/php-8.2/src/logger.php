<?php

class Logger {
    const TYPE_ERROR = 'error';
    const TYPE_LOG = 'log';

    public string $id = '';
    private bool $enabled = false;
    private $includesNativeInfo = false;

    private mixed $streamLogs = null;
    private mixed $streamErrors = null;

    public function __construct(?string $status = null, ?string $id = null) {
        $this->enabled = (!empty($status) ? $status : 'enabled') === 'enabled';

        if($this->enabled) {
            $this->id = !empty($id) ? $id : (\getenv('OPEN_RUNTIMES_ENV') === 'development' ? 'dev' : $this->generateId());

            $this->streamLogs = \fopen('/mnt/logs/' . $this->id . '_logs.log', 'a');
            $this->streamErrors = \fopen('/mnt/logs/' . $this->id . '_errors.log', 'a');
        }
    }

    public function write(mixed $message, string $type = Logger::TYPE_LOG, bool $isNative = false): void {
        if(!$this->enabled) {
          return;
        }
    
        if($isNative && !$this->includesNativeInfo) {
          $this->includesNativeInfo = true;
          $this->write('Native logs detected. Use context.log() or context.error() for better experience.');
        }
    
        $stream = $type == Logger::TYPE_ERROR ? $this->streamErrors : $this->streamLogs;
    
        $stringLog = "";

        if(\is_array($message) || \is_object($message)) {
            $stringLog = \json_encode($message);
        } else {
            $stringLog = \strval($message);
        }
    
        \fwrite($stream, $stringLog . "\n");
    }

    public function end(): void {
        if(!$this->enabled) {
          return;
        }
    
        $this->enabled = false;

        \fclose($this->streamLogs);
        \fclose($this->streamErrors);
    }


    public function overrideNativeLogs(): void {
        if(!$this->enabled) {
            return;
        }

        \ob_start();
    }

    public function revertNativeLogs(): void {
        if(!$this->enabled) {
            return;
        }

        $customStd = ob_get_clean();

        if(!empty($customStd)) {
            $this->write($customStd, Logger::TYPE_LOG, true);
        }
    }
    
    private function generateId(int $padding = 7): string {
        $uniqid = \uniqid();

        if ($padding > 0) {
            $bytes = \random_bytes(\max(1, (int)\ceil(($padding / 2)))); // one byte expands to two chars
            $uniqid .= \substr(\bin2hex($bytes), 0, $padding);
        }

        return $uniqid;
    }
}