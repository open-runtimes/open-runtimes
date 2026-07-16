<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

class BuildManifest extends TestCase
{
    private string $root;
    private string $helpers;
    private string $repoHelpers;
    private string $output;

    protected function setUp(): void
    {
        $this->root = \sys_get_temp_dir() . '/open-runtimes-build-manifest-' . \bin2hex(\random_bytes(6));
        $this->output = $this->root . '/output';
        \mkdir($this->output, 0777, true);
        $this->repoHelpers = \dirname(__DIR__) . '/helpers';
        $this->helpers = $this->root . '/helpers';
        \mkdir($this->helpers, 0777, true);
        \copy($this->repoHelpers . '/build-manifest.sh', $this->helpers . '/build-manifest.sh');
    }

    protected function tearDown(): void
    {
        $this->removePath($this->root);
    }

    public function testNoOpWhenManifestPathUnset(): void
    {
        \file_put_contents($this->output . '/index.html', 'html');

        $result = $this->runManifest([]);

        self::assertSame(0, $result['code']);
        self::assertSame('', $result['output']);
        self::assertSame([], \glob($this->root . '/*.json'));
    }

    public function testWritesVersionedManifestOfOutputFiles(): void
    {
        \file_put_contents($this->output . '/index.html', 'html');
        \mkdir($this->output . '/server', 0777, true);
        \file_put_contents($this->output . '/server/entry.mjs', 'mjs');

        $result = $this->runManifest();

        self::assertSame(0, $result['code']);
        $manifest = $this->readManifest();
        self::assertSame(1, $manifest['version']);
        $files = $manifest['files'];
        \sort($files);
        self::assertSame(['index.html', 'server/entry.mjs'], $files);
        self::assertStringContainsString('Build manifest written. (2 files)', $result['output']);
        self::assertFileDoesNotExist($this->manifestPath() . '.tmp');
    }

    public function testPrunesNodeModulesAndExcludesArchiveArtifacts(): void
    {
        \file_put_contents($this->output . '/index.html', 'html');
        \mkdir($this->output . '/node_modules/pkg', 0777, true);
        \file_put_contents($this->output . '/node_modules/pkg/index.js', 'js');
        \file_put_contents($this->output . '/code.tar.gz', 'tar');
        \file_put_contents($this->output . '/code.sqfs', 'sqfs');

        $result = $this->runManifest();

        self::assertSame(0, $result['code']);
        self::assertSame(['index.html'], $this->readManifest()['files']);
    }

    public function testEscapesSpecialCharactersInFileNames(): void
    {
        $name = "we\"ird\\na\tme\r.html";
        \file_put_contents($this->output . '/' . $name, 'html');

        $result = $this->runManifest();

        self::assertSame(0, $result['code']);
        self::assertSame([$name], $this->readManifest()['files']);
    }

    public function testRelativeManifestPathWritesFileNotDirectory(): void
    {
        \file_put_contents($this->output . '/index.html', 'html');

        $result = $this->runManifest(['OPEN_RUNTIMES_BUILD_MANIFEST' => 'manifest.json']);

        self::assertSame(0, $result['code']);
        $path = $this->output . '/manifest.json';
        self::assertFileExists($path);
        self::assertDirectoryDoesNotExist($path);
        $manifest = \json_decode((string) \file_get_contents($path), true);
        self::assertSame(['index.html'], $manifest['files']);
    }

    public function testCapsFileCount(): void
    {
        foreach (\range(1, 5) as $i) {
            \file_put_contents($this->output . '/file-' . $i . '.html', 'html');
        }

        $result = $this->runManifest([
            'OPEN_RUNTIMES_BUILD_MANIFEST' => $this->manifestPath(),
            'OPEN_RUNTIMES_BUILD_MANIFEST_MAX_FILES' => '2',
        ]);

        self::assertSame(0, $result['code']);
        self::assertCount(2, $this->readManifest()['files']);
    }

    public function testEmptyOutputWritesEmptyFileList(): void
    {
        $result = $this->runManifest();

        self::assertSame(0, $result['code']);
        $manifest = $this->readManifest();
        self::assertSame(1, $manifest['version']);
        self::assertSame([], $manifest['files']);
        self::assertStringContainsString('Build manifest written. (0 files)', $result['output']);
    }

    public function testBuildLifecycleWritesManifestAfterPackaging(): void
    {
        $build = \file_get_contents($this->repoHelpers . '/lifecycle/build.sh');

        self::assertStringContainsString('build-manifest.sh', $build);
        self::assertLessThan(\strpos($build, 'build-manifest.sh'), \strpos($build, 'Build packaging finished'));
        self::assertLessThan(\strpos($build, 'Build finished'), \strpos($build, 'build-manifest.sh'));
    }

    private function manifestPath(): string
    {
        return $this->root . '/manifest.json';
    }

    private function readManifest(): array
    {
        self::assertFileExists($this->manifestPath());
        $manifest = \json_decode((string) \file_get_contents($this->manifestPath()), true);
        self::assertIsArray($manifest);

        return $manifest;
    }

    /**
     * Runs build-manifest.sh with the output directory as the working
     * directory, mirroring how lifecycle/build.sh invokes it. Passing null
     * for OPEN_RUNTIMES_BUILD_MANIFEST leaves it unset.
     */
    private function runManifest(?array $env = null): array
    {
        $env ??= ['OPEN_RUNTIMES_BUILD_MANIFEST' => $this->manifestPath()];

        $command = 'cd ' . \escapeshellarg($this->output) . ' && /bin/bash ' . \escapeshellarg($this->helpers . '/build-manifest.sh');

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
            'output' => $this->stripAnsi($output),
        ];
    }

    private function stripAnsi(string $output): string
    {
        return \preg_replace('/(?:\x1B|\\\\e)\[[0-9;]*m/', '', $output) ?? $output;
    }

    private function removePath(string $path): void
    {
        if (\is_dir($path)) {
            foreach (\scandir($path) ?: [] as $entry) {
                if ($entry === '.' || $entry === '..') {
                    continue;
                }
                $this->removePath($path . '/' . $entry);
            }
            \rmdir($path);

            return;
        }

        if (\file_exists($path)) {
            \unlink($path);
        }
    }
}
