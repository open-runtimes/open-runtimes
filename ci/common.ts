// Shared runtime-configuration resolution for ci/index.ts (CI matrix),
// ci/test.ts (test driver) and ci/bake.ts (image targets).

// @ts-ignore
import data from './runtimes.toml';

export interface RuntimeCommands {
    install: string;
    start: string;
}

export interface RuntimeFormatter {
    check: string;
    write: string;
    prepare: string;
}

export interface Runtime {
    runtime?: {
        name: string;
        version: string;
    };
    test: string;
    versions?: string[];
    tools: string;
    commands: RuntimeCommands;
    formatter: RuntimeFormatter;
    report_size?: boolean;
    test_resource_dir?: string;
    shared?: string;
    build?: Record<string, any>;

    // Website metadata
    output: string;

    // Serverless metadata
    entry: string;
    entry_no_export: string;
}

export interface Entry {
    ID: string;
    RUNTIME: string;
    VERSION: string;
    TEST_CLASS: string;
    ENTRYPOINT: string;
    ENTRYPOINT_NO_EXPORT: string;
    OUTPUT_DIRECTORY: string;
    INSTALL_COMMAND: string;
    START_COMMAND: string;
    TOOLS: string;
    FORMATTER_CHECK: string;
    FORMATTER_WRITE: string;
    FORMATTER_PREPARE: string;
    ENFORCED_RUNTIME: string;
    ENFORCED_VERSION: string;
    REPORT_SIZE: boolean;

    // Derived (not consumed by the GitHub matrix itself)
    RUNTIME_FOLDER: string;
    VERSION_FOLDER: string;
    TEST_RESOURCE_DIR: string;
    BAKE_TARGET: string;
    RUN_FORMATTER: boolean;
    COMPOSE_PROFILES: string[];
}

export const runtimes: Record<string, Runtime> = data;

export function generateRuntimeObject(runtime: Runtime, key: string): Entry[] {
    const entries: Entry[] = [];

    (runtime.versions ?? ['']).forEach((version) => {
        const isMjs = key === 'node' && version.includes('mjs');
        const cleanedVersion = cleanVersion(version);
        const id = `${key}${version ? `-${version}` : ''}`;
        const reportSize = isMjs ? false : (runtime.report_size !== false);

        const enforcedRuntime = runtime.runtime?.name ?? '';
        const enforcedVersion = runtime.runtime?.version ?? '';

        // Folder math (matches the original ci-helpers.sh derivation):
        // python-ml + 3.13 -> folder python, version dir ml-3.13
        let runtimeFolder = key.includes('-') ? key.slice(0, key.lastIndexOf('-')) : key;
        let versionFolder = `${key}-${cleanedVersion}`.slice(runtimeFolder.length + 1);
        if (enforcedRuntime) {
            runtimeFolder = enforcedRuntime;
        }
        if (enforcedVersion) {
            versionFolder = enforcedVersion;
        }

        const bakeTarget = (enforcedRuntime
            ? `${enforcedRuntime}-${enforcedVersion}`
            : `${key}-${cleanedVersion}`
        ).replaceAll('.', '_');

        // Formatter runs only for the latest version of a runtime, never for
        // enforced-runtime aliases (matches the original ci_tests.sh gate)
        const runFormatter = !enforcedRuntime && version === (runtime.versions ?? [])[0];

        const profiles: string[] = [];
        if (runtime.entry_no_export) {
            profiles.push('no-export');
        }
        if ((runtime.test ?? '').startsWith('SSR/') && ['node', 'bun', 'deno'].includes(runtimeFolder)) {
            profiles.push('cleanup-variants');
        }

        entries.push({
            ID: id,
            RUNTIME: key,
            VERSION: cleanedVersion,
            TEST_CLASS: runtime.test,
            ENTRYPOINT: isMjs ? 'tests.mjs' : runtime.entry,
            ENTRYPOINT_NO_EXPORT: runtime.entry_no_export,
            OUTPUT_DIRECTORY: runtime.output,
            INSTALL_COMMAND: runtime.commands.install,
            START_COMMAND: runtime.commands.start,
            TOOLS: runtime.tools,
            FORMATTER_CHECK: runtime.formatter.check,
            FORMATTER_WRITE: runtime.formatter.write,
            FORMATTER_PREPARE: runtime.formatter.prepare,
            ENFORCED_RUNTIME: enforcedRuntime,
            ENFORCED_VERSION: enforcedVersion,
            REPORT_SIZE: reportSize,

            RUNTIME_FOLDER: runtimeFolder,
            VERSION_FOLDER: versionFolder,
            TEST_RESOURCE_DIR: runtime.test_resource_dir ?? '',
            BAKE_TARGET: bakeTarget,
            RUN_FORMATTER: runFormatter,
            COMPOSE_PROFILES: profiles,
        });
    });

    return entries;
}

export function allEntries(): Entry[] {
    const entries: Entry[] = [];
    for (const [key, runtime] of Object.entries(runtimes)) {
        entries.push(...generateRuntimeObject(runtime, key));
    }
    return entries;
}

// Resolve a test ID like "node-25", "node-latest", "node", "python-ml-3.13"
// or "next-js" to its matrix entry.
export function resolveEntry(id: string): Entry {
    const entries = allEntries();

    const exact = entries.find((entry) => entry.ID === id);
    if (exact) {
        return exact;
    }

    // "node" or "node-latest" -> first (latest) version of that runtime
    const key = id.endsWith('-latest') ? id.slice(0, -'-latest'.length) : id;
    const first = entries.find((entry) => entry.RUNTIME === key);
    if (first) {
        return first;
    }

    throw new Error(`Unknown runtime ID: ${id}`);
}

function cleanVersion(version: string): string {
    return version.replace('-mjs', '');
}
