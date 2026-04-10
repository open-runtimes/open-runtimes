# Runtime Performance Baseline & Optimization Results

## Optimized Results (branch `chore-optimize`)

Cross-host averaged (M1 + M2), wrk 4.2.0, true cold starts (docker rm + run), `/tmp/logs/` cleaned between runtimes.

```
Runtime                  Cold     P50       P99     HotAvg     RPS     RAM        Image
---------------------   ------   ------   ------   ------   ------   ---------   --------
Bun 1.3                   216   289 us    32 ms    30 ms    8,979   1.06 GiB    366 MB
Node 25                 1,046   292 us     4 ms    40 ms    8,190   1.79 GiB    1.85 GB
Go 1.26                   264   364 us    67 ms    50 ms    7,891   731 MiB     392 MB
Java 25 (Netty)           585   282 us    71 ms    42 ms    7,750   1.03 GiB    1.05 GB
Kotlin 2.3 (Netty)        996   322 us    74 ms    42 ms    7,622   986 MiB     989 MB
PHP 8.4                   259   634 us    71 ms    56 ms    5,557   535 MiB     363 MB
Swift 6.2                 548   630 us   800 us    51 ms    5,054   461 MiB     5.38 GB
C++ 23                    258   287 us    65 ms    57 ms    4,728   463 MiB     698 MB
Dart 3.11                 312   455 us    69 ms    62 ms    4,335   449 MiB     1.21 GB
Deno 2.6                  428   520 us    56 ms    68 ms    3,762   710 MiB     250 MB
Ruby 4.0                1,169   1.44 ms    3 ms    85 ms    3,078   572 MiB     541 MB
Python 3.14             1,580   564 us   692 us   110 ms    2,327   289 MiB     556 MB
.NET 10                   443   422 us     5 ms   204 ms    1,248   199 MiB     1.08 GB
```

## Improvement vs Pre-Optimization Baseline

```
Runtime              Base RPS → Opt RPS    ΔRPS    Base Cold → Opt Cold    ΔCold
-----------          --------   -------   ------   ---------   --------   ------
Ruby 4.0                  590 →   3,078   +422%       1,480 →    1,169     -21%
PHP 8.4                 2,749 →   5,557   +102%         454 →      259     -43%
Deno 2.6                1,881 →   3,762   +100%         688 →      428     -38%
Go 1.26                 4,117 →   7,891    +92%         382 →      264     -31%
Node 25                 4,974 →   8,190    +65%       1,811 →    1,046     -42%
Python 3.14             1,484 →   2,327    +57%       2,814 →    1,580     -44%
Bun 1.3                 6,000 →   8,979    +50%         427 →      216     -49%
C++ 23                  3,256 →   4,728    +45%         472 →      258     -45%
Swift 6.2               3,634 →   5,054    +39%         624 →      548     -12%
Kotlin 2.3 (Netty)      6,964 →   7,622     +9%       1,025 →      996      -3%
Java 25 (Netty)         7,364 →   7,750     +5%         693 →      585     -16%
Dart 3.11               4,252 →   4,335     +2%         383 →      312     -19%
.NET 10                 1,238 →   1,248     +1%         515 →      443     -14%

9/13 runtimes achieved 30%+ RPS improvement.
12/13 runtimes achieved faster cold starts.
```

## What Changed Per Runtime

```
Runtime     Optimization
----------  -----------------------------------------------------------
Ruby        Puma multi-worker (nproc workers, 16 threads), skip Async
            fiber when no timeout, cache enforced headers at startup
PHP         Swoole worker_num tuned to 2x CPU, TCP_NODELAY enabled,
            cache enforced headers/secret/entrypoint at startup
Deno        Cache enforced headers, secret, entrypoint at module level
Go          Cache enforced headers in init(), set GOMAXPROCS, add
            server ReadTimeout/WriteTimeout/IdleTimeout
Node        Cache user module require() at startup, cache enforced
            headers and secret at module level
Python      Cache module import (load once, not per-request), cache
            enforced headers, remove Poetry dep, add keep-alive
Bun         Cache enforced headers, secret, entrypoint at module level
C++         Cache enforced headers in initCache() called once in main()
Swift       Cache enforced headers and secret as nonisolated globals
Java        Netty with epoll, DefaultEventExecutorGroup (128 threads),
            TCP_NODELAY, cached class loading and enforced headers
Kotlin      Netty with epoll, DefaultEventExecutorGroup + coroutines,
            TCP_NODELAY, cached class loading and enforced headers
Dart        Cache enforced headers as top-level final variables
.NET        Cache enforced headers as static readonly fields
```

## Methodology

### Hosts

```
Host  CPU       RAM     OS                          Docker
----  ---       ---     --                          ------
M1    8 vCPU    16 GB   Ubuntu 24.04 (6.8.0-101)    29.3.0
M2    8 vCPU    16 GB   Ubuntu 24.04 (6.8.0-71)     29.4.0
```

### Load generator

wrk 4.2.0 compiled from source (identical binary on both hosts).

### Per-runtime procedure

```
 1. Clean /tmp/logs/ (prevents ext4 htree exhaustion)
 2. Build Docker image (CI assembly + docker build)
 3. Build user code (test function via helpers/build.sh)
 4. Cold start (5x): docker rm -f + docker run (true cold start)
    Measured from docker inspect StartedAt to first 200 on /__opr/health
 5. Warm-up: 500 requests
 6. Warm latency: wrk -t1 -c1 -d10s --latency (P50, P99)
 7. Throughput: wrk -t8 -c256 -d30s (RPS, hot avg latency)
 8. RAM: docker stats --no-stream (post-load)
 9. Cleanup: docker rm -f + 2s settle
```

### Key findings during benchmarking

1. **Log directory poisoning**: `/tmp/logs/` accumulated millions of files across runs
   (2 files per request). This caused ext4 htree exhaustion, crashing Node/Bun and
   degrading all runtimes by 40-60%. Fix: clean between runtimes.

2. **Netty keep-alive**: Initial Netty implementation used ChannelFutureListener.CLOSE
   on every response, forcing TCP reconnection per request. Fix: HttpUtil.isKeepAlive().

3. **Netty event loop blocking**: Running user functions directly on Netty event loop
   threads blocked all connections sharing that thread. Fix: DefaultEventExecutorGroup.

4. **Virtual thread dispatch overhead**: Using Executors.newVirtualThreadPerTaskExecutor()
   added ~300us per request vs direct execution. Not worth it for this workload.

### HTTP frameworks

```
Runtime     Framework                      Concurrency Model
----------  ----------------------------   ---------------------------
Java        Netty 4.1.115 + epoll           DefaultEventExecutorGroup
Kotlin      Netty 4.1.115 + epoll           DefaultEventExecutorGroup + coroutines
Node        Micro (http server)             Event loop
Bun         Bun.serve()                     Event loop
Deno        Oak (middleware)                Event loop
Python      aiohttp + Gunicorn              Workers (1 per CPU) + async
Go          net/http                        Goroutines
PHP         Swoole HTTP Server              Coroutines (multi-process)
Ruby        Puma + Sinatra                  Workers (nproc) + threads (16)
Swift       Vapor (SwiftNIO)                Event loop + async/await
C++         Drogon                          Async callbacks
.NET        Kestrel (ASP.NET)               Async/await
Dart        shelf + shelf_router            Isolates
```
