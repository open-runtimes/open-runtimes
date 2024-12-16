// @ts-ignore
import data from './runtimes.toml'
import { appendFileSync } from 'fs';

interface RuntimeCommands {
    install: string,
    start: string
}

interface RuntimeFormatter {
    check: string,
    write: string,
    prepare: string,
}

interface Runtime {
    runtime?: {
        name: string
        version: string
    },
    test: string,
    versions?: string[],
    tools: string,
    commands: RuntimeCommands,
    formatter: RuntimeFormatter,

    // Website metadata
    output: string,

    // Serverless metadata
    entry: string,
}

let perRuntime = true;
const runtimes: Record<string, Runtime> = data;
const matrix: Record<string, any>[] = [];
const files = (process.env.ALL_CHANGED_FILES ?? '').split(' ');
const folders = getFolders(files);

for(const file of files) {
    if(file.startsWith("runtimes/")) {
        continue;
    } else if(file.startsWith("tests/resources/functions/")) {
        continue;
    } else {
        perRuntime = false;
        break;
    }
}

let isGlobal = false;

// Global folders
if(['ci', '.github', 'helpers'].some(path => folders.includes(path))) {
    isGlobal = true;
}

// Global files
if(['tests/Base.php', 'tests/CSR.php', 'tests/SSR.php', 'tests/Serverless.php'].some(file => files.includes(file))) {
    isGlobal = true;
}

// Test all in case of CI or Test file changes
if (isGlobal) {
    perRuntime = false;
}

if (perRuntime) {
    folders.forEach((folder) => {
        if (runtimes[folder] !== undefined) {
            matrix.push(...generateRuntimeObject(runtimes[folder], folder));
        }
    });
} else {
    for (const [key, runtime] of Object.entries(runtimes)) {
        matrix.push(...generateRuntimeObject(runtime, key));
    }
}

const uniqueMatrix: Record<string, any>[] = [];
const uniqueKeys: string[] = [];
for(const entry of matrix) {
    const key = entry.ID;

    if(!uniqueKeys.includes(key)) {
        uniqueKeys.push(key);
        uniqueMatrix.push(entry);
    }
}

appendFileSync(process.env.GITHUB_OUTPUT ?? '', `matrix=${JSON.stringify({include: uniqueMatrix})}`);

function generateRuntimeObject(runtime: Runtime, key: string) {
    const object: Record<string, any>[] = [];

    (runtime.versions ?? ['']).forEach((version) => {
        if (key === 'node' && version.includes('mjs')) {
            runtime.entry = "tests.mjs";
        }

        const id = `${key}${version ? `-${version}` : ''}`;

        object.push({
            ID: id,
            RUNTIME: key,
            VERSION: cleanVersion(version),
            TEST_CLASS: runtime.test,
            ENTRYPOINT: runtime.entry,
            OUTPUT_DIRECTORY: runtime.output,
            INSTALL_COMMAND: runtime.commands.install,
            START_COMMAND: runtime.commands.start,
            TOOLS: runtime.tools,
            FORMATTER_CHECK: runtime.formatter.check,
            FORMATTER_PREPARE: runtime.formatter.prepare,
            ENFORCED_RUNTIME: runtime.runtime?.name ?? "",
            ENFORCED_VERSION: runtime.runtime?.version ?? "",
        })
    });

    return object;
}

function cleanVersion(version: string): string {
    return version.replace('-mjs', '');
}

function getFolders(changes: string[]): string[] {
    const folders = new Set();

    changes.forEach((change) => {
        let folder = change;

        if (change.indexOf('runtimes') === 0) {
            folder = change.replace('runtimes/', '');
        }

        if (change.indexOf('tests') === 0) {
            folder = change.replace('tests/resources/functions/', '');
        }

        folders.add(folder.slice(0, folder.indexOf('/')));
    });

    return Array.from(folders) as string[];
}
