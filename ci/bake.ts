// Generates docker-bake.json from the [*.build] tables in ci/runtimes.toml.
//
//   bun ci/bake.ts          regenerate docker-bake.json
//   bun ci/bake.ts --check  fail if docker-bake.json is out of date, or if the
//                           build version list drifts from the test `versions`
//
// docker-bake.json is committed; together with docker-bake.hcl (variables +
// _base target) it makes `docker buildx bake <target|group>` work with no
// generation step.

// @ts-ignore
import data from './runtimes.toml';
import { existsSync, readFileSync, writeFileSync } from 'fs';
import { dirname, join } from 'path';

interface BuildVersion {
    base: string;
    build_base?: string;
    args?: Record<string, string>;
    platforms?: string[];
    version_dir?: string;
}

interface BuildConfig {
    image?: string;
    runtime_dir?: string;
    platforms?: string[];
    versions: Record<string, BuildVersion>;
}

const DEFAULT_PLATFORMS = ['linux/amd64', 'linux/arm64'];

const repoRoot = join(dirname(Bun.main), '..');
const outputPath = join(repoRoot, 'docker-bake.json');

const groups: Record<string, { targets: string[] }> = { default: { targets: [] } };
const targets: Record<string, any> = {};
const errors: string[] = [];

// Resolve `INCLUDE ./path` directives in a family Dockerfile by inlining the
// referenced fragment (path + ".dockerfile"). The resolved content is embedded
// in the bake file via `dockerfile-inline`, so no third-party BuildKit
// frontend is needed.
const dockerfileCache: Record<string, string> = {};

function resolveDockerfile(runtimeDir: string): string {
    if (dockerfileCache[runtimeDir]) {
        return dockerfileCache[runtimeDir];
    }

    const path = join(repoRoot, 'runtimes', runtimeDir, 'Dockerfile');
    const resolved = readFileSync(path, 'utf-8')
        .split('\n')
        .map((line) => {
            const include = line.match(/^INCLUDE\s+\.\/(\S+)\s*$/);
            if (!include) {
                return line;
            }
            const fragment = join(repoRoot, `${include[1]}.dockerfile`);
            if (!existsSync(fragment)) {
                errors.push(`${path}: INCLUDE target not found: ${fragment}`);
                return line;
            }
            return readFileSync(fragment, 'utf-8').trimEnd();
        })
        .join('\n')
        // Bake parses JSON strings as HCL templates — escape Dockerfile
        // ${ARG} interpolations so they reach BuildKit untouched (function
        // replacements avoid the special meaning of $ in replacement strings)
        .replaceAll('${', () => '$${')
        .replaceAll('%{', () => '%%{');

    dockerfileCache[runtimeDir] = resolved;
    return resolved;
}

for (const [key, runtime] of Object.entries<any>(data)) {
    const build: BuildConfig | undefined = runtime.build;
    if (!build) {
        continue;
    }

    const runtimeDir = build.runtime_dir ?? key;
    const image = build.image ?? key;

    groups[key] = { targets: [] };
    groups.default.targets.push(key);

    // Hidden per-family target carrying the resolved Dockerfile; version
    // targets inherit it (underscore prefix keeps it out of all groups)
    targets[`_${runtimeDir}`] = {
        'dockerfile-inline': resolveDockerfile(runtimeDir),
    };

    // The published versions must stay aligned with the test `versions` list
    // (ignoring test-only -mjs variants).
    const testVersions = new Set<string>((runtime.versions ?? []).map((v: string) => v.replace('-mjs', '')));
    const buildVersions = Object.keys(build.versions);
    for (const v of buildVersions) {
        if (!testVersions.has(v)) {
            errors.push(`${key}: build version "${v}" missing from test versions list`);
        }
    }
    for (const v of testVersions) {
        if (!buildVersions.includes(v)) {
            errors.push(`${key}: test version "${v}" missing from [${key}.build.versions]`);
        }
    }

    for (const [version, cfg] of Object.entries(build.versions)) {
        const targetName = `${key}-${version}`.replaceAll('.', '_');
        groups[key].targets.push(targetName);

        const versionDir = `runtimes/${runtimeDir}/versions/${cfg.version_dir ?? version}`;
        const args: Record<string, string> = { BASE_IMAGE: cfg.base };
        if (cfg.build_base) {
            args.BUILD_BASE_IMAGE = cfg.build_base;
        }
        Object.assign(args, cfg.args ?? {});

        targets[targetName] = {
            inherits: ['_base', `_${runtimeDir}`],
            contexts: {
                helpers: './helpers',
                shared: runtime.shared ? `./runtimes/${runtime.shared}` : './docker/empty',
                latest: `./runtimes/${runtimeDir}/versions/latest`,
                version: existsSync(join(repoRoot, versionDir)) ? `./${versionDir}` : './docker/empty',
            },
            args,
            platforms: cfg.platforms ?? build.platforms ?? DEFAULT_PLATFORMS,
            // Mutable tag (consumed by appwrite/appwrite) plus an immutable
            // SHA-pinned tag for cloud. SHA_SUFFIX is empty by default, so the
            // second tag collapses onto the first and bake dedupes them.
            tags: [
                `\${REGISTRY}/${image}:v5-${version}\${TAG_SUFFIX}`,
                `\${REGISTRY}/${image}:v5-${version}\${TAG_SUFFIX}\${SHA_SUFFIX}`,
            ],
        };
    }
}

if (errors.length > 0) {
    console.error('runtimes.toml is inconsistent:');
    for (const error of errors) {
        console.error(`  - ${error}`);
    }
    process.exit(1);
}

const output = JSON.stringify({ group: groups, target: targets }, null, 2) + '\n';

if (process.argv.includes('--check')) {
    const current = existsSync(outputPath) ? readFileSync(outputPath, 'utf-8') : '';
    if (current !== output) {
        console.error('docker-bake.json is out of date. Run: bun ci/bake.ts');
        process.exit(1);
    }
    console.log('docker-bake.json is up to date.');
} else {
    writeFileSync(outputPath, output);
    console.log(`Wrote ${Object.keys(targets).length} targets to docker-bake.json`);
}
