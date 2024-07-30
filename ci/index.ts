// @ts-ignore
import data from './runtimes.toml'
import {exec} from 'child_process';
import {appendFileSync} from 'fs';

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
    entry: string,
    versions: string[],
    commands: RuntimeCommands,
    formatter: RuntimeFormatter
}

let perRuntime = true;
const runtimes: Record<string, Runtime> = data;
const matrix: Record<string, any>[] = [];
const folders = getFolders((process.env.ALL_CHANGED_FILES ?? '').split(' '));

// Test all in case of CI or Test file changes
if (folders.includes('ci') || folders.includes('.github')) {
    for (const [key, runtime] of Object.entries(runtimes)) {
        matrix.push(...generateRuntimeObject(runtime, key));
        perRuntime = false;
    }
}

if (perRuntime) {
    folders.forEach((folder) => {
        if (runtimes[folder] !== undefined) {
            matrix.push(...generateRuntimeObject(runtimes[folder], folder));
        }
    });
}

appendFileSync(process.env.GITHUB_OUTPUT, `matrix=${JSON.stringify({include: matrix})}`);

function generateRuntimeObject(runtime: Runtime, key: string) {
    const object: Record<string, any>[] = [];

    runtime.versions.forEach((version) => {
        if (key === 'node' && version.includes('mjs')) {
            runtime.entry = "tests.mjs";
        }


        object.push({
            ID: `${key}-${version}`,
            RUNTIME: key,
            VERSION: cleanVersion(version),
            ENTRYPOINT: runtime.entry,
            INSTALL_COMMAND: runtime.commands.install,
            START_COMMAND: runtime.commands.start,
            FORMATTER_CHECK: runtime?.formatter?.check, // If question marks are here, it's leftover. Remove them please
            FORMATTER_PREPARE: runtime?.formatter?.prepare, // If question marks are here, it's leftover. Remove them please
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
