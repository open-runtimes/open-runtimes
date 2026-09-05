# Runtime shutdown

Start runtime containers through `bash helpers/start.sh "<start command>"`.
The launcher uses Tini to forward signals and reap orphaned processes, and a
supervisor to manage the runtime process group. This covers extraction,
preparation, the HTTP server, shell wrappers, and ordinary child processes.
The image stop signal is explicitly `SIGTERM`.

On SIGTERM (or SIGINT, normalized to SIGTERM), the supervisor signals the
runtime group once. Servers stop accepting connections and use their HTTP
server's shutdown mechanism. The launcher waits for the server and its output
reader, preserving final log lines and the server's exit status. Repeated
signals do not restart the grace period.

`OPEN_RUNTIMES_SHUTDOWN_TIMEOUT` sets the hard shutdown deadline in seconds:
**30 by default**, valid range **1–86400**. After the deadline the supervisor
sends SIGKILL to the runtime process group, including a server stuck in
synchronous user code. Native server-specific deadlines can finish shutdown
earlier. Java/Kotlin, .NET, Python, and PHP use this setting for their server
shutdown timeout as well.

Configure the container manager to allow more than this deadline, for example:

```sh
docker stop --time 35 <container>
```

For Compose use `stop_grace_period: 35s`; in Kubernetes, account for any preStop
hook time in `terminationGracePeriodSeconds`. Docker's shorter default stop
window can otherwise cut off a drain before the runtime deadline expires.

| Runtime | Shutdown mechanism |
| --- | --- |
| Node and bundled SSR adapters | Close listener, finish active responses, close idle keep-alive sockets, flush stdout/stderr |
| Bun | Stop listener and wait for pending requests/WebSockets, including older Bun versions whose stop method returns void |
| Deno | Abort Oak's listener and await its shutdown |
| Go | `http.Server.Shutdown`, awaited before main returns |
| Rust | Stop accepting, signal Hyper connections to drain, await connection tasks |
| Dart and Flutter's test static server | Close the HTTP listener without forcing active connections; keep the event loop alive for responses |
| Java/Kotlin | JVM shutdown hook stops Javalin/Jetty; Java also shuts down its executor |
| Python | Gunicorn/aiohttp's native graceful worker shutdown |
| Ruby | Puma's native SIGTERM shutdown |
| PHP | Swoole's native SIGTERM shutdown with asynchronous worker draining |
| .NET | ASP.NET Core's host shutdown |
| Swift | Vapor's native shutdown, followed by application resource cleanup |
| C++ | Drogon's native signal handling and event-loop shutdown |
| Static | Static Web Server's native graceful shutdown |

Custom SSR start commands and framework-generated standalone servers receive
signals, but their own server code must implement draining. End custom shell
wrappers with `exec` where possible. Keep subprocesses in the runtime process
group; intentionally detached sessions are outside the supervisor's deadline.
HTTP upgrades and background work may consume the entire grace period.

SIGKILL cannot be caught, delayed, or cleaned up after. Killing the container's
PID 1 terminates the container and its remaining processes. There is no
promise of a response, a flushed log, or a completed external write after a
forced kill. Function authors should make external writes transactional or
idempotent and tolerate interrupted executions; a retry can repeat work.

## Verification

Every `make test ID=...` now checks the real runtime container's stop behavior
after PHPUnit and fails on a forced kill or unexpected exit code.

For detailed launcher and Node HTTP regressions, build a Node runtime image
and run:

```sh
make image ID=node
python3 -m unittest discover -s tests/shutdown -v
```

Set `SHUTDOWN_TEST_IMAGE` to use another Node runtime image containing Tini and
its runtime dependencies. Tests cover in-flight responses, idle keep-alive,
repeated signals, subprocess draining, log flushing, startup failure status,
invalid deadlines, hard deadlines, SIGINT, and SIGKILL. Each test creates and
removes its own container.
