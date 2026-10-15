<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

/**
 * Behavioral coverage for helpers/lifecycle/write-build-metadata.sh.
 *
 * Flutter build-prepare creates `.open-runtimes/` (with dart_defines.json) under
 * the build root; packaging must turn that into the metadata file without
 * discarding real application output or packaging an empty tree.
 */
class BuildMetadataCollision extends TestCase
{
    private string $root;

    private string $helpers;

    protected function setUp(): void
    {
        $this->root = \sys_get_temp_dir() . '/open-runtimes-build-metadata-' . \bin2hex(\random_bytes(6));
        \mkdir($this->root, 0777, true);
        $this->helpers = \dirname(__DIR__) . '/helpers/lifecycle';
    }

    protected function tearDown(): void
    {
        $this->removePath($this->root);
    }

    public function testBuildLifecycleSourcesWriteBuildMetadataHelper(): void
    {
        $build = \file_get_contents($this->helpers . '/build.sh');

        self::assertStringContainsString(
            'helpers/lifecycle/write-build-metadata.sh',
            $build
        );
    }

    public function testFlutterDefinesDirectoryIsReplacedWithMetadataFile(): void
    {
        \mkdir($this->root . '/.open-runtimes', 0777, true);
        \file_put_contents($this->root . '/.open-runtimes/dart_defines.json', '{}');
        \file_put_contents($this->root . '/index.html', '<html></html>');

        $result = $this->runWriteMetadata([
            'OPEN_RUNTIMES_ENTRYPOINT' => 'index.html',
            'OPEN_RUNTIMES_CLEANUP' => 'none',
        ]);

        self::assertSame(0, $result['code'], $result['output']);
        self::assertFileExists($this->root . '/.open-runtimes');
        self::assertTrue(\is_file($this->root . '/.open-runtimes'));
        self::assertFileExists($this->root . '/index.html');
        $metadata = \file_get_contents($this->root . '/.open-runtimes');
        self::assertStringContainsString('OPEN_RUNTIMES_ENTRYPOINT=index.html', $metadata);
        self::assertStringContainsString('OPEN_RUNTIMES_CLEANUP=none', $metadata);
    }

    public function testUnexpectedOpenRuntimesDirectoryContentsAreRejected(): void
    {
        \mkdir($this->root . '/.open-runtimes', 0777, true);
        \file_put_contents($this->root . '/.open-runtimes/app-owned.txt', 'keep me');
        \file_put_contents($this->root . '/index.html', '<html></html>');

        $result = $this->runWriteMetadata([
            'OPEN_RUNTIMES_ENTRYPOINT' => 'index.html',
        ]);

        self::assertNotSame(0, $result['code'], $result['output']);
        self::assertStringContainsString('unexpected contents', $this->stripAnsi($result['output']));
        self::assertDirectoryExists($this->root . '/.open-runtimes');
        self::assertFileExists($this->root . '/.open-runtimes/app-owned.txt');
        self::assertFileExists($this->root . '/index.html');
    }

    public function testOnlyFlutterDefinesDirectoryFailsAsEmptyOutput(): void
    {
        \mkdir($this->root . '/.open-runtimes', 0777, true);
        \file_put_contents($this->root . '/.open-runtimes/dart_defines.json', '{}');

        $result = $this->runWriteMetadata([
            'OPEN_RUNTIMES_ENTRYPOINT' => 'index.html',
        ]);

        self::assertNotSame(0, $result['code'], $result['output']);
        self::assertStringContainsString('No build output found', $this->stripAnsi($result['output']));
    }

    /**
     * @param array<string, string> $env
     * @return array{code: int, output: string}
     */
    private function runWriteMetadata(array $env): array
    {
        $lib = $this->helpers . '/lib.sh';
        $script = $this->helpers . '/write-build-metadata.sh';
        $command = 'cd ' . \escapeshellarg($this->root)
            . ' && . ' . \escapeshellarg($lib)
            . ' && . ' . \escapeshellarg($script);

        $descriptorSpec = [
            1 => ['pipe', 'w'],
            2 => ['pipe', 'w'],
        ];

        $process = \proc_open(['/bin/bash', '-c', $command], $descriptorSpec, $pipes, null, $env + [
            'PATH' => '/bin:/usr/bin',
        ]);

        if (!\is_resource($process)) {
            self::fail('Failed to start process.');
        }

        $output = \stream_get_contents($pipes[1]) . \stream_get_contents($pipes[2]);
        \fclose($pipes[1]);
        \fclose($pipes[2]);

        return [
            'code' => \proc_close($process),
            'output' => $output,
        ];
    }

    private function stripAnsi(string $output): string
    {
        return \preg_replace('/(?:\x1B|\\e)\[[0-9;]*m/', '', $output) ?? $output;
    }

    private function removePath(string $path): void
    {
        if (!\file_exists($path) && !\is_link($path)) {
            return;
        }

        if (\is_file($path) || \is_link($path)) {
            \unlink($path);

            return;
        }

        $iterator = new \RecursiveIteratorIterator(
            new \RecursiveDirectoryIterator($path, \FilesystemIterator::SKIP_DOTS),
            \RecursiveIteratorIterator::CHILD_FIRST
        );

        foreach ($iterator as $file) {
            $file->isDir() ? \rmdir($file->getPathname()) : \unlink($file->getPathname());
        }

        \rmdir($path);
    }
}
