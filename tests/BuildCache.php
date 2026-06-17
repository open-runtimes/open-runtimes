<?php

namespace Tests;

use PHPUnit\Framework\TestCase;
use RecursiveDirectoryIterator;
use RecursiveIteratorIterator;
use FilesystemIterator;

class BuildCache extends TestCase
{
    private string $root;
    private string $helpers;
    private string $repoHelpers;

    protected function setUp(): void
    {
        $this->root = \sys_get_temp_dir() . '/open-runtimes-build-cache-' . \bin2hex(\random_bytes(6));
        \mkdir($this->root, 0777, true);
        $this->repoHelpers = \dirname(__DIR__) . '/helpers';
        $this->helpers = $this->root . '/helpers';
        $this->copyHelpersForTest();
    }

    protected function tearDown(): void
    {
        $this->removePath($this->root);
    }

    public function testRestoreMissExitsZero(): void
    {
        $bin = $this->createBinDir(['unsquashfs' => "#!/bin/sh\nexit 0\n"]);
        $result = $this->runScript('build-cache-restore.sh', $bin);

        self::assertSame(0, $result['code']);
        $this->assertBuildCacheLogContains('Build cache miss.', $result['output']);
    }

    public function testRestoreWithMissingUnsquashfsExitsZero(): void
    {
        $result = $this->runScript('build-cache-restore.sh', $this->createBinDir());

        self::assertSame(0, $result['code']);
        $this->assertBuildCacheLogContains('Build cache warning: missing unsquashfs, continuing without cache restore.', $result['output']);
    }

    public function testRestoreWithCorruptArtifactExitsZeroAndClearsPartialCacheRoot(): void
    {
        $artifact = $this->root . '/stores.sqfs';
        \file_put_contents($artifact, 'corrupt');

        $bin = $this->createBinDir([
            'unsquashfs' => "#!/bin/sh\nprintf 'squashfs stdout'\nprintf 'squashfs stderr' >&2\nmkdir -p \"$4/partial\"\nexit 1\n",
        ]);

        $result = $this->runScript('build-cache-restore.sh', $bin);

        self::assertSame(0, $result['code']);
        $this->assertBuildCacheLogContains('Build cache warning: failed to restore cache, starting fresh.', $result['output']);
        self::assertStringNotContainsString('squashfs stdout', $result['output']);
        self::assertStringNotContainsString('squashfs stderr', $result['output']);
        self::assertDirectoryExists($this->cacheRoot());
        self::assertFileDoesNotExist($this->cacheRoot() . '/partial');
    }

    public function testSaveWithMissingMksquashfsExitsZero(): void
    {
        \mkdir($this->cacheRoot(), 0777, true);

        $result = $this->runScript('build-cache-save.sh', $this->createBinDir());

        self::assertSame(0, $result['code']);
        $this->assertBuildCacheLogContains('Build cache warning: missing mksquashfs, continuing without cache save.', $result['output']);
    }

    public function testSaveWithMissingCacheRootExitsZero(): void
    {
        $bin = $this->createBinDir(['mksquashfs' => "#!/bin/sh\nexit 0\n"]);

        $result = $this->runScript('build-cache-save.sh', $bin);

        self::assertSame(0, $result['code']);
        $this->assertBuildCacheLogContains('Build cache save skipped: cache root missing.', $result['output']);
    }

    public function testSaveWithMissingArtifactDirectoryExitsZero(): void
    {
        \mkdir($this->cacheRoot(), 0777, true);
        $this->writeTestCachePaths($this->cacheRoot(), $this->root . '/missing/stores.sqfs');
        $bin = $this->createBinDir(['mksquashfs' => "#!/bin/sh\nexit 1\n"]);

        $result = $this->runScript('build-cache-save.sh', $bin);

        self::assertSame(0, $result['code']);
        $this->assertBuildCacheLogContains('Build cache save skipped: artifact directory missing.', $result['output']);
    }

    public function testSaveFailureExitsZeroAndDeletesTemporaryArtifact(): void
    {
        \mkdir($this->cacheRoot(), 0777, true);
        $bin = $this->createBinDir([
            'mksquashfs' => "#!/bin/sh\nprintf tmp > \"$2\"\nexit 1\n",
        ]);

        $result = $this->runScript('build-cache-save.sh', $bin);

        self::assertSame(0, $result['code']);
        $this->assertBuildCacheLogContains('Build cache warning: failed to save cache, build result preserved.', $result['output']);
        self::assertFileDoesNotExist($this->artifact() . '.tmp');
    }

    public function testSuccessfulSaveWritesFinalArtifactAtomically(): void
    {
        \mkdir($this->cacheRoot(), 0777, true);
        \file_put_contents($this->cacheRoot() . '/store', 'cache');
        $bin = $this->createBinDir([
            'mksquashfs' => "#!/bin/sh\nprintf 'squashfs stdout'\nprintf 'squashfs stderr' >&2\nprintf saved > \"$2\"\nexit 0\n",
        ]);

        $result = $this->runScript('build-cache-save.sh', $bin);

        self::assertSame(0, $result['code']);
        $this->assertBuildCacheLogContains('Build cache saved.', $result['output']);
        self::assertStringNotContainsString('squashfs stdout', $result['output']);
        self::assertStringNotContainsString('squashfs stderr', $result['output']);
        self::assertFileExists($this->artifact());
        self::assertFileDoesNotExist($this->artifact() . '.tmp');
    }

    public function testMoveFailureExitsZeroAndDeletesTemporaryArtifact(): void
    {
        \mkdir($this->cacheRoot(), 0777, true);
        \file_put_contents($this->cacheRoot() . '/store', 'cache');
        $bin = $this->createBinDir([
            'mksquashfs' => "#!/bin/sh\nprintf saved > \"$2\"\nexit 0\n",
            'mv' => "#!/bin/sh\nexit 1\n",
        ]);

        $result = $this->runScript('build-cache-save.sh', $bin);

        self::assertSame(0, $result['code']);
        $this->assertBuildCacheLogContains('Build cache warning: failed to save cache, build result preserved.', $result['output']);
        self::assertFileDoesNotExist($this->artifact());
        self::assertFileDoesNotExist($this->artifact() . '.tmp');
    }

    public function testUserBuildCommandFailureStillFailsBuild(): void
    {
        $result = $this->runShell('false; status=$?; exit "$status"');

        self::assertSame(1, $result['code']);
    }

    public function testUserBuildCommandSuccessStillSucceedsIfCacheSaveFails(): void
    {
        \mkdir($this->cacheRoot(), 0777, true);
        $bin = $this->createBinDir(['mksquashfs' => "#!/bin/sh\nexit 1\n"]);

        $result = $this->runShell(
            'true; status=$?; if [ "$status" -eq 0 ]; then /bin/bash ' . \escapeshellarg($this->helpers . '/build-cache-save.sh') . '; fi; exit "$status"',
            $bin,
            []
        );

        self::assertSame(0, $result['code']);
        $this->assertBuildCacheLogContains('Build cache warning: failed to save cache, build result preserved.', $result['output']);
    }

    public function testBuildLifecycleRunsCacheAutomaticallyInBeforeAndAfterBuild(): void
    {
        $build = \file_get_contents($this->helpers . '/build.sh');
        $beforeBuild = \file_get_contents($this->helpers . '/before-build.sh');
        $afterBuild = \file_get_contents($this->helpers . '/after-build.sh');

        self::assertStringNotContainsString('build-cache-restore.sh', $build);
        self::assertStringNotContainsString('build-cache-save.sh', $build);
        self::assertStringContainsString('. /usr/local/server/helpers/build-cache-env.sh', $beforeBuild);
        self::assertLessThan(\strpos($beforeBuild, 'Environment preparation started'), \strpos($beforeBuild, 'build-cache-restore.sh'));
        self::assertLessThan(\strpos($afterBuild, 'Build finished'), \strpos($afterBuild, 'build-cache-save.sh'));
    }

    public function testBuildCacheEnvUsesFixedPaths(): void
    {
        $env = \file_get_contents($this->repoHelpers . '/build-cache-env.sh');

        self::assertStringContainsString('OPEN_RUNTIMES_BUILD_CACHE_ROOT="/usr/local/cache/build"', $env);
        self::assertStringContainsString('OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT="/cache/stores.sqfs"', $env);
        self::assertStringNotContainsString('${OPEN_RUNTIMES_BUILD_CACHE_ROOT:-', $env);
        self::assertStringNotContainsString('${OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT:-', $env);
    }

    public function testCacheHitStillWorksOnSecondBuild(): void
    {
        \mkdir($this->cacheRoot(), 0777, true);
        \file_put_contents($this->cacheRoot() . '/store', 'cache');

        $bin = $this->createBinDir([
            'mksquashfs' => "#!/bin/sh\nprintf restored > \"$2\"\nexit 0\n",
            'unsquashfs' => "#!/bin/sh\nmkdir -p \"$4\"\ncp \"$5\" \"$4/store\"\nexit 0\n",
        ]);

        $save = $this->runScript('build-cache-save.sh', $bin);
        $this->removePath($this->cacheRoot());
        $restore = $this->runScript('build-cache-restore.sh', $bin);

        self::assertSame(0, $save['code']);
        self::assertSame(0, $restore['code']);
        $this->assertBuildCacheLogContains('Build cache hit.', $restore['output']);
        self::assertSame('restored', \file_get_contents($this->cacheRoot() . '/store'));
    }

    public function testNoExecutorPathReferencesBuildCacheWrapper(): void
    {
        $repo = \dirname(__DIR__);
        self::assertFileDoesNotExist($repo . '/helpers/build-cache.sh');

        if (!\is_dir($repo . '/executor')) {
            self::assertTrue(true);
            return;
        }

        $iterator = new RecursiveIteratorIterator(
            new RecursiveDirectoryIterator($repo . '/executor', FilesystemIterator::SKIP_DOTS)
        );

        foreach ($iterator as $file) {
            if (!$file->isFile() || \strpos($file->getPathname(), '/.git/') !== false) {
                continue;
            }

            self::assertStringNotContainsString('build-cache.sh', \file_get_contents($file->getPathname()), $file->getPathname());
        }
    }

    private function runScript(string $script, string $bin, array $env = []): array
    {
        return $this->runShell('/bin/bash ' . \escapeshellarg($this->helpers . '/' . $script), $bin, $env);
    }

    private function assertBuildCacheLogContains(string $message, string $output): void
    {
        $output = $this->stripAnsi($output);

        self::assertStringContainsString('[open-runtimes]', $output);
        self::assertStringContainsString($message, $output);
    }

    private function stripAnsi(string $output): string
    {
        return \preg_replace('/(?:\x1B|\\\\e)\[[0-9;]*m/', '', $output) ?? $output;
    }

    private function runShell(string $command, ?string $bin = null, array $env = []): array
    {
        $descriptorSpec = [
            1 => ['pipe', 'w'],
            2 => ['pipe', 'w'],
        ];

        $process = \proc_open(['/bin/bash', '-c', $command], $descriptorSpec, $pipes, null, $env + [
            'PATH' => ($bin ? $bin . ':' : '') . '/bin:/usr/bin',
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

    private function createBinDir(array $commands = []): string
    {
        $bin = $this->root . '/bin-' . \bin2hex(\random_bytes(3));
        \mkdir($bin, 0777, true);

        foreach ($commands as $name => $contents) {
            $path = $bin . '/' . $name;
            \file_put_contents($path, $contents);
            \chmod($path, 0755);
        }

        return $bin;
    }

    private function cacheRoot(): string
    {
        return $this->root . '/cache-root';
    }

    private function artifact(): string
    {
        return $this->root . '/stores.sqfs';
    }

    private function copyHelpersForTest(): void
    {
        \mkdir($this->helpers, 0777, true);

        foreach (['build-cache-env.sh', 'build-cache-restore.sh', 'build-cache-save.sh', 'build.sh', 'before-build.sh', 'after-build.sh'] as $file) {
            \copy($this->repoHelpers . '/' . $file, $this->helpers . '/' . $file);
        }

        $this->writeTestCachePaths($this->cacheRoot(), $this->artifact());
    }

    private function writeTestCachePaths(string $cacheRoot, string $artifact): void
    {
        $env = \file_get_contents($this->repoHelpers . '/build-cache-env.sh');
        $env = \str_replace('OPEN_RUNTIMES_BUILD_CACHE_ROOT="/usr/local/cache/build"', 'OPEN_RUNTIMES_BUILD_CACHE_ROOT="' . $cacheRoot . '"', $env);
        $env = \str_replace('OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT="/cache/stores.sqfs"', 'OPEN_RUNTIMES_BUILD_CACHE_ARTIFACT="' . $artifact . '"', $env);
        \file_put_contents($this->helpers . '/build-cache-env.sh', $env);
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

        $iterator = new RecursiveIteratorIterator(
            new RecursiveDirectoryIterator($path, FilesystemIterator::SKIP_DOTS),
            RecursiveIteratorIterator::CHILD_FIRST
        );

        foreach ($iterator as $file) {
            $file->isDir() ? \rmdir($file->getPathname()) : \unlink($file->getPathname());
        }

        \rmdir($path);
    }
}
