// Reuse the fixture's three-second timeout action to verify draining without
// adding a special shutdown endpoint to every language's test function.
export async function checkShutdown(container: string, baseUrl: string, drain: boolean): Promise<void> {
    const docker = (...args: string[]) => {
        const result = Bun.spawnSync(['docker', ...args], { stderr: 'inherit' });
        if (result.exitCode !== 0) throw new Error(`docker ${args.join(' ')} failed`);
        return result.stdout.toString();
    };

    let response: Promise<{ status: number; body: string }> | undefined;
    if (drain) {
        const since = new Date().toISOString();
        response = fetch(baseUrl, {
            method: 'POST',
            headers: { 'x-open-runtimes-secret': 'test-secret-key', 'x-action': 'timeout' },
            signal: AbortSignal.timeout(15_000),
        }).then(async (res) => ({ status: res.status, body: await res.text() }));
        // Keep early request failures handled while waiting for the start marker.
        response.catch(() => {});

        const deadline = Date.now() + 10_000;
        while (true) {
            const logs = docker('logs', '--since', since, container);
            if (logs.includes('Timeout end.')) throw new Error('Request finished before shutdown could be tested');
            if (logs.includes('Timeout start.')) break;
            if (Date.now() >= deadline) throw new Error('Shutdown test request did not start');
            await Bun.sleep(50);
        }
    }

    docker('stop', '--time', '35', container);
    const code = docker('inspect', '--format', '{{.State.ExitCode}}', container).trim();
    if (!['0', '143'].includes(code)) throw new Error(`Runtime did not shut down cleanly (exit ${code})`);
    if (response) {
        const result = await response;
        if (result.status !== 200 || result.body !== 'Successful response.') {
            throw new Error(`Active request was not drained: ${JSON.stringify(result)}`);
        }
    }
}
