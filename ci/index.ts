// Generates the GitHub Actions test matrix from ci/runtimes.toml, filtered by
// changed files. Each matrix entry is a runtime ID consumed by `bun ci/test.ts`.

import { appendFileSync } from 'fs';
import { runtimes, generateRuntimeObject, type Entry } from './common';

let perRuntime = true;
const matrix: Entry[] = [];
const files = (process.env.ALL_CHANGED_FILES ?? '').split(' ');
const folders = getFolders(files);

for (const file of files) {
    if (file.startsWith("runtimes/")) {
        continue;
    } else if (file.startsWith("tests/resources/functions/")) {
        continue;
    } else {
        perRuntime = false;
        break;
    }
}

let isGlobal = false;

// Global folders
if (['ci', '.github', 'helpers', 'docker'].some(path => folders.includes(path))) {
    isGlobal = true;
}

// Global files
if (['tests/Base.php', 'tests/CSR.php', 'tests/SSR.php', 'tests/Serverless.php', 'tests/compose.yaml', 'docker-bake.hcl', 'docker-bake.json'].some(file => files.includes(file))) {
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

        const dependingRuntimes: string[] = [];
        Object.keys(runtimes).forEach((name) => {
            if ((runtimes[name].runtime?.name ?? '') === folder) {
                dependingRuntimes.push(name);
            }
        });

        for (const runtime of dependingRuntimes) {
            matrix.push(...generateRuntimeObject(runtimes[runtime], runtime));
        }
    });
} else {
    for (const [key, runtime] of Object.entries(runtimes)) {
        matrix.push(...generateRuntimeObject(runtime, key));
    }
}

const uniqueMatrix: Record<string, any>[] = [];
const uniqueKeys: string[] = [];
for (const entry of matrix) {
    const key = entry.ID;

    if (!entry.TEST_CLASS) {
        continue;
    }

    if (!uniqueKeys.includes(key)) {
        uniqueKeys.push(key);
        uniqueMatrix.push({ ID: entry.ID, REPORT_SIZE: entry.REPORT_SIZE });
    }
}
appendFileSync(process.env.GITHUB_OUTPUT ?? '', `matrix=${JSON.stringify({ include: uniqueMatrix })}`);

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
