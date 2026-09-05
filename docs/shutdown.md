# Runtime shutdown

Start containers through `bash helpers/start.sh "<start command>"`. Tini
forwards signals to the runtime process group and reaps orphaned children.
The startup shell waits for the server and its log pipeline, preserving final
output and the server's exit code. Server hooks use `exec` to avoid extra shells.

On SIGTERM, servers use their HTTP framework's graceful shutdown support to
stop accepting work and finish active requests. Explicit handlers cover the
runtimes and bundled SSR adapters that do not handle this themselves.
Custom SSR commands must implement their own request draining.

**The container manager owns the shutdown deadline.** For example:

```sh
docker stop --time 35 <container>
```

Use `stop_grace_period` in Compose or `terminationGracePeriodSeconds` in
Kubernetes. Allow enough time for requests and any preStop hook; native server
shutdown timeouts may finish earlier. There is no separate runtime watchdog
or timeout setting. SIGINT is forwarded unchanged, so its behavior follows
the underlying server.

After the deadline, the container manager uses SIGKILL. SIGKILL cannot be
caught: responses, logs, and external writes may be interrupted. External
writes should tolerate retries and interrupted executions.

## Verification

`make test ID=...` checks that the runtime container stops without a forced
kill after PHPUnit. For detailed launcher and Node HTTP regressions:

```sh
make image ID=node
python3 -m unittest discover -s tests/shutdown -v
```

Set `SHUTDOWN_TEST_IMAGE` to use another Node image with Tini and runtime
dependencies. Tests cover active requests, idle keep-alive, child processes,
final logs, repeated signals, exit codes, and Docker's forced-stop deadline.
