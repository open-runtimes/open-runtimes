import * as core from '@actions/core';
// @ts-ignore
import data from '../../runtimes.toml'

interface RuntimeCommands {
    install: string,
    start: string
}

interface Runtime {
    entry: string,
    versions: string[],
    commands: RuntimeCommands,
}

const runtimes: Record<string, Runtime> = data;
const matrix: Record<string, any>[] = [];

const changedFiles = JSON.parse(process.env.ALL_CHANGED_FILES ?? '[]');
console.log(changedFiles);
core.setOutput('matrix', JSON.stringify(matrix));
core.setOutput('length', matrix.length.toString());


function generateRuntimeObject(runtime: Runtime, key: string) {
    const object: Record<string, any>[] = [];

    runtime.versions.forEach((version) => {
        object.push({
            ID: `${key}-${version}`,
            RUNTIME: key,
            VERSION: version,
            ENTRYPOINT: runtime.entry,
            INSTALL_COMMAND: runtime.commands.install,
            START_COMMAND: runtime.commands.start,

        })
    });

    return object;
}
