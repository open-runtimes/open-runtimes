<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

class BuildCompression extends TestCase
{
    private string $root;

    protected function setUp(): void
    {
        $this->root = \sys_get_temp_dir() . '/open-runtimes-build-compression-' . \bin2hex(\random_bytes(6));
        \mkdir($this->root, 0777, true);
    }

    protected function tearDown(): void
    {
        $this->removePath($this->root);
    }

    public function testDefaultCompressionRemainsGzip(): void
    {
        $result = $this->runCompressionSelection([]);

        self::assertSame(0, $result['code']);
        self::assertStringContainsString('METHOD=gzip', $result['output']);
    }

    public function testAutoCompressionSkipsCompressionForSmallOutputs(): void
    {
        $result = $this->runCompressionSelection([
            'OPEN_RUNTIMES_BUILD_COMPRESSION' => 'auto',
        ]);

        self::assertSame(0, $result['code']);
        self::assertStringContainsString('METHOD=none', $result['output']);
    }

    public function testAutoCompressionUsesGzipForLargeOutputs(): void
    {
        \file_put_contents($this->root . '/large.bin', \str_repeat('x', 6 * 1024 * 1024));

        $result = $this->runCompressionSelection([
            'OPEN_RUNTIMES_BUILD_COMPRESSION' => 'auto',
        ]);

        self::assertSame(0, $result['code']);
        self::assertStringContainsString('METHOD=gzip', $result['output']);
    }

    public function testExplicitSquashfsCompressionIsSupported(): void
    {
        $result = $this->runCompressionSelection([
            'OPEN_RUNTIMES_BUILD_COMPRESSION' => 'squashfs',
        ]);

        self::assertSame(0, $result['code']);
        self::assertStringContainsString('METHOD=squashfs', $result['output']);
    }

    public function testBuildLifecyclePackagesSquashfsAndKeepsLegacyTarBranches(): void
    {
        $build = \file_get_contents(\dirname(__DIR__) . '/helpers/lifecycle/build.sh');

        self::assertStringContainsString('"$COMPRESSION_METHOD" = "squashfs"', $build);
        self::assertStringContainsString('mksquashfs . "$OUTPUT_DIR/code.sqfs"', $build);
        self::assertStringContainsString('-comp lz4 -b 1M -noappend -no-xattrs -no-progress', $build);
        self::assertStringContainsString('"$COMPRESSION_METHOD" = "none"', $build);
        self::assertStringContainsString('"$COMPRESSION_METHOD" = "zstd"', $build);
        self::assertStringContainsString('"$OUTPUT_DIR/code.tar.gz"', $build);
    }

    public function testExtractionPrefersSquashfsAndKeepsLegacyTarFallbacks(): void
    {
        $extract = \file_get_contents(\dirname(__DIR__) . '/helpers/lifecycle/extract.sh');

        self::assertStringContainsString('68737173) echo "squashfs"', $extract);
        self::assertStringContainsString('squashfs) unsquashfs -q -f -d "$dest" "$archive"', $extract);
        self::assertLessThan(\strpos($extract, '/mnt/code/code.tar'), \strpos($extract, '/mnt/code/code.sqfs'));
        self::assertStringContainsString('code.sqfs | code.tar | code.tar.gz | code.gz | .extracted', $extract);
    }

    private function runCompressionSelection(array $env): array
    {
        $script = \dirname(__DIR__) . '/helpers/lifecycle/compression.sh';
        \file_put_contents($this->root . '/file.txt', 'contents');

        $command = 'cd ' . \escapeshellarg($this->root) . ' && . ' . \escapeshellarg($script) . ' && printf "METHOD=%s\n" "$COMPRESSION_METHOD"';

        return $this->runShell($command, $env);
    }

    private function runShell(string $command, array $env): array
    {
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
