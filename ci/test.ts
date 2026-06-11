// Test driver for a single runtime. Replaces ci_tests.sh / tests.sh /
// formatter.sh / ci-helpers.sh / ci-cleanup.sh.
//
//   bun ci/test.ts node-25                 build image, stage fixtures, run tests
//   bun ci/test.ts node                    latest version of a runtime
//   bun ci/test.ts next-js                 SSR alias (enforced runtime)
//   bun ci/test.ts node-25 --skip-image    reuse existing open-runtimes/test-runtime
//   bun ci/test.ts node-25 --format-write  run formatters in write mode and exit
//
// Flow: bake image -> formatter check (latest only) -> stage fixtures ->
// tools -> build (+ variants by profile) -> serve trio -> phpunit -> down.

import { existsSync, cpSync, rmSync, mkdirSync, writeFileSync, readdirSync } from 'fs';
import { join, dirname } from 'path';
import { resolveEntry, type Entry } from './common';

const repoRoot = join(dirname(Bun.main), '..');
const args = process.argv.slice(2);
const id = args.find((arg) => !arg.startsWith('--'));
if (!id) {
    console.error('Usage: bun ci/test.ts <runtime-id> [--skip-image] [--image-only] [--skip-formatter] [--format-write]');
    process.exit(1);
}

const entry = resolveEntry(id);
const composeEnv: Record<string, string> = {
    ...process.env as Record<string, string>,
    RUNTIME: entry.RUNTIME,
    VERSION: entry.VERSION,
    ENTRYPOINT: entry.ENTRYPOINT ?? '',
    ENTRYPOINT_NO_EXPORT: entry.ENTRYPOINT_NO_EXPORT ?? '',
    OUTPUT_DIRECTORY: entry.OUTPUT_DIRECTORY ?? '',
    INSTALL_COMMAND: entry.INSTALL_COMMAND ?? '',
    START_COMMAND: entry.START_COMMAND ?? '',
    TOOLS: entry.TOOLS ?? 'true',
    TEST_CLASS: entry.TEST_CLASS,
    COMPOSE_PROFILES: entry.COMPOSE_PROFILES.join(','),
};

const compose = ['docker', 'compose', '-f', join(repoRoot, 'tests/compose.yaml')];

function run(cmd: string[], env: Record<string, string> = composeEnv): void {
    console.log(`+ ${cmd.join(' ')}`);
    const result = Bun.spawnSync(cmd, { cwd: repoRoot, env, stdout: 'inherit', stderr: 'inherit' });
    if (result.exitCode !== 0) {
        console.error(`Command failed (exit ${result.exitCode}): ${cmd.join(' ')}`);
        process.exit(result.exitCode ?? 1);
    }
}

function buildImage(): void {
    run([
        'docker', 'buildx', 'bake', entry.BAKE_TARGET, '--load',
        '--set', `*.platform=${process.env.TEST_PLATFORM ?? 'linux/x86_64'}`,
        '--set', '*.tags=open-runtimes/test-runtime',
    ]);
}

function runFormatter(mode: 'check' | 'write'): void {
    const command = mode === 'check' ? entry.FORMATTER_CHECK : entry.FORMATTER_WRITE;
    const dirs = [join(repoRoot, 'runtimes', entry.RUNTIME_FOLDER)];
    const testResources = join(repoRoot, 'tests/resources/functions', entry.RUNTIME_FOLDER);
    if (existsSync(testResources)) {
        dirs.push(testResources);
    }
    for (const dir of dirs) {
        run([...compose, 'run', '--rm', 'formatter'], {
            ...composeEnv,
            FORMAT_DIR: dir,
            FORMATTER_PREPARE: entry.FORMATTER_PREPARE ?? 'true',
            FORMATTER_CMD: command ?? 'true',
        });
    }
}

function stageFixtures(): void {
    const testsDir = join(repoRoot, 'tests');
    const runtimeDir = join(testsDir, '.runtime');
    rmSync(runtimeDir, { recursive: true, force: true });
    mkdirSync(runtimeDir, { recursive: true });

    const functions = join(testsDir, 'resources/functions');
    if (entry.TEST_RESOURCE_DIR) {
        cpSync(join(functions, entry.TEST_RESOURCE_DIR), runtimeDir, { recursive: true });
    } else if (entry.ENFORCED_RUNTIME) {
        cpSync(join(functions, entry.RUNTIME), runtimeDir, { recursive: true });
    } else {
        cpSync(join(functions, entry.RUNTIME_FOLDER, 'latest'), runtimeDir, { recursive: true });
        const versionDir = join(functions, entry.RUNTIME_FOLDER, entry.VERSION_FOLDER);
        if (existsSync(versionDir)) {
            cpSync(versionDir, runtimeDir, { recursive: true });
        }
    }

    // Seed telemetry timings (download metrics are produced by the executor,
    // not the runtime; tests assert their presence)
    const telemetry = join(testsDir, 'resources/telemetry');
    mkdirSync(telemetry, { recursive: true });
    writeFileSync(join(telemetry, 'timings.txt'), 'local_download=0.200\nremote_download=10.560\n');

    // Prevent Docker from creating the archive path as a directory
    writeFileSync(join(runtimeDir, 'code.tar.gz'), '');

    // Variant build directories (copies of the staged fixture, excluding the
    // variant dirs themselves to avoid recursive copies)
    const variantDirs = ['no-export-build', 'modclean-disabled-build', 'nft-build'];
    const stageVariant = (variant: string) => {
        const dir = join(runtimeDir, variant, 'src');
        mkdirSync(dir, { recursive: true });
        for (const item of readdirSync(runtimeDir)) {
            if (variantDirs.includes(item)) {
                continue;
            }
            cpSync(join(runtimeDir, item), join(dir, item), { recursive: true });
        }
    };
    if (entry.COMPOSE_PROFILES.includes('no-export')) {
        stageVariant('no-export-build');
    }
    if (entry.COMPOSE_PROFILES.includes('cleanup-variants')) {
        stageVariant('modclean-disabled-build');
        stageVariant('nft-build');
    }

    rmSync('/tmp/logs', { recursive: true, force: true });
    mkdirSync('/tmp/logs', { recursive: true });
}

function down(): void {
    Bun.spawnSync([...compose, 'down', '--remove-orphans'], { cwd: repoRoot, env: composeEnv });
    // Also remove any open-runtimes-* containers not created by compose
    // (e.g. leftovers from the legacy ci_tests.sh or manual runs)
    const list = Bun.spawnSync(['docker', 'ps', '-aq', '-f', 'name=open-runtimes-'], { env: composeEnv });
    const ids = list.stdout.toString().trim().split('\n').filter(Boolean);
    if (ids.length > 0) {
        Bun.spawnSync(['docker', 'rm', '--force', ...ids], { env: composeEnv });
    }
}

// ─── Flow ────────────────────────────────────────────────────────────────────

if (!args.includes('--skip-image')) {
    buildImage();
}

if (args.includes('--image-only')) {
    process.exit(0);
}

if (args.includes('--format-write')) {
    runFormatter('write');
    process.exit(0);
}

if (entry.RUN_FORMATTER && !args.includes('--skip-formatter')) {
    console.log('Running formatter ...');
    runFormatter('check');
} else if (!entry.RUN_FORMATTER) {
    console.log(`Skipping formatter. Formatter runs only for the latest version, and not for enforced-runtime aliases.`);
}

console.log('Staging test fixtures ...');
stageFixtures();
down();
Bun.spawnSync(['docker', 'network', 'create', 'openruntimes'], { env: composeEnv });

console.log('Testing tools ...');
run([...compose, 'run', '--rm', 'tools']);

console.log('Running build ...');
run([...compose, 'run', '--rm', 'build']);
if (entry.COMPOSE_PROFILES.includes('no-export')) {
    console.log('Building no-export entrypoint test ...');
    run([...compose, 'run', '--rm', 'build-no-export']);
}
if (entry.COMPOSE_PROFILES.includes('cleanup-variants')) {
    console.log('Building cleanup-disabled baseline ...');
    run([...compose, 'run', '--rm', 'build-baseline']);
    console.log('Building NFT-enabled test ...');
    run([...compose, 'run', '--rm', 'build-nft']);
}

console.log('Starting runtime servers ...');
run([...compose, 'up', '-d']);

console.log('Running tests ...');
run([...compose, 'run', '--rm', 'phpunit']);

down();
console.log('Done.');
