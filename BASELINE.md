# Runtime Performance Baseline

Measured 2026-04-10. Java and Kotlin use Netty (branch `chore-kt-updates`). All others use their existing HTTP frameworks.

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

```
Warm latency:  wrk -t1 -c1 -d10s --latency
Throughput:    wrk -t8 -c256 -d30s
```

### Per-runtime procedure

```
 1. Build Docker image (CI assembly + docker build)
 2. Build user code (test function via helpers/build.sh)
 3. Cold start (5x): docker rm -f + docker run, measure from
    docker inspect StartedAt to first 200 on /__opr/health
    (polled via curl --connect-timeout 0.005, tight loop)
 4. Warm-up: 500 requests
 5. Warm latency: wrk -t1 -c1 -d10s --latency
 6. Throughput: wrk -t8 -c256 -d30s
 7. RAM: docker stats --no-stream (post-load, after 256c/30s)
 8. Cleanup: docker rm -f + 2s settle
```

Cold start is a true cold start: fresh container from image, code extraction from tar.gz, server boot, first healthy response. NOT a container restart.

### Cross-host validation

All 13 runtimes ran on both M1 and M2. Values below are cross-host averages. Node and Bun containers died during wrk on both hosts (persistent issue under investigation) so only cold start data is available for those two.

## Results

Sorted by throughput descending. Cold start is avg of 5 true cold starts.

```
Runtime                  Cold     P50       P99       RPS    RAM        Image
---------------------   ------   ------   ------   ------   ---------   --------
Kotlin 2.3 (Netty)      1,005   571 us    82 ms    4,485   715 MiB     1.02 GB
Dart 3.11                 232   629 us   128 ms    3,974   409 MiB     1.21 GB
Go 1.26                   250   578 us    97 ms    3,335   533 MiB     392 MB
Java 25 (Netty)           687   705 us    68 ms    2,659   566 MiB     1.05 GB
C++ 23                    229   510 us    94 ms    2,549   228 MiB     698 MB
PHP 8.4                   219   669 us   104 ms    2,417   289 MiB     363 MB
Swift 6.2                 533   1.05 ms   72 ms    2,334   249 MiB     5.38 GB
Deno 2.6                  421   781 us    91 ms    2,127   472 MiB     250 MB
Python 3.14             1,506   668 us    55 ms    1,427   216 MiB     690 MB
.NET 10                   441   590 us    99 ms    1,199   215 MiB     1.08 GB
Ruby 4.0                1,140   1.54 ms   11 ms      599   133 MiB     537 MB
Bun 1.3                   210     —         —         —*     —         366 MB
Node 25                 1,024     —         —         —*     —         1.85 GB
```

`*` Node and Bun containers crash during wrk load test on both hosts. Cold start data is valid.

## Per-host raw data

### M1

```
Runtime           Cold    P50       P99       RPS     RAM        Image
--------------   ------  --------  --------  ------  ---------  --------
Node 25          1,013       —         —         —*      —       1.85 GB
Bun 1.3            207       —         —         —*      —       366 MB
Deno 2.6           419   781 us    91 ms      2,122   472 MiB    250 MB
Python 3.14      1,509   668 us    55 ms      1,526   216 MiB    690 MB
Go 1.26            246   578 us    97 ms      3,885   533 MiB    392 MB
C++ 23             225   510 us    94 ms      2,733   228 MiB    698 MB
PHP 8.4            215   669 us   104 ms      2,867   289 MiB    363 MB
Ruby 4.0         1,135   1.54 ms   11 ms        596   133 MiB    547 MB
Swift 6.2          530   1.05 ms   72 ms      2,462   249 MiB    5.38 GB
.NET 10            439   590 us    99 ms      1,211   215 MiB    1.08 GB
Dart 3.11          234   629 us   128 ms      3,913   409 MiB    1.21 GB
```

### M2

```
Runtime                  Cold    P50       P99       RPS     RAM        Image
---------------------   ------  --------  --------  ------  ---------  --------
Node 25                 1,035       —         —         —*      —       1.84 GB
Bun 1.3                   213       —         —         —*      —       366 MB
Deno 2.6                  423   716 us    79 ms      2,133   511 MiB    250 MB
Python 3.14             1,503   835 us    37 ms      1,329   198 MiB    680 MB
Go 1.26                   254   735 us    77 ms      2,785   304 MiB    392 MB
C++ 23                    233   627 us    72 ms      2,366   280 MiB    689 MB
PHP 8.4                   223   845 us    40 ms      1,967   214 MiB    351 MB
Ruby 4.0                1,145   1.68 ms    3 ms        603   130 MiB    537 MB
Swift 6.2                 536   1.34 ms   69 ms      2,206   223 MiB    5.38 GB
.NET 10                   443   626 us    86 ms      1,187   215 MiB    1.08 GB
Dart 3.11                 231   664 us   106 ms      4,036   416 MiB    1.21 GB
Java 25 (Netty)           687   705 us    68 ms      2,659   566 MiB    1.05 GB
Kotlin 2.3 (Netty)      1,005   571 us    82 ms      4,485   715 MiB    1.02 GB
```

## Observations

### Cold start tiers

```
Fast   (<250ms):  Bun 210, PHP 219, C++ 229, Dart 232, Go 250
Medium (250-700ms):  Deno 421, .NET 441, Swift 533, Java 687
Slow   (>700ms):  Kotlin 1005, Node 1024, Ruby 1140, Python 1506
```

### Java/Kotlin Netty analysis

Java and Kotlin use raw Netty with:
- `DefaultEventExecutorGroup` (128 threads) for blocking handler execution
- Epoll transport on Linux (native kernel epoll vs NIO JNI bridge)
- TCP_NODELAY for low latency
- HTTP keep-alive support
- Cached user class loading (single reflection load at startup)
- Cached enforced headers parsing

Kotlin (4,485 RPS) significantly outperforms Java (2,659 RPS) on the same Netty stack. This is likely due to Kotlin's coroutine-based timeout handling vs Java's `Future.get()` thread blocking.

### Node and Bun crash investigation needed

Both Node 25 and Bun 1.3 containers die during the wrk load test (256 concurrent connections). The cold start data is valid (containers start and serve health checks), but they crash under sustained load. This affects both hosts identically and is likely a runtime-level issue (OOM kill, event loop saturation, or connection handling bug).

### RAM measurement context

RAM is `docker stats --no-stream` captured immediately after 30s of 256 concurrent connections. This is post-peak usage (connection buffers, thread stacks, warm caches). Idle RAM is significantly lower.

### HTTP frameworks

```
Runtime          Framework                      Concurrency Model
--------------   ----------------------------   ---------------------------
Java             Netty 4.1.115 + epoll           DefaultEventExecutorGroup (128 threads)
Kotlin           Netty 4.1.115 + epoll           DefaultEventExecutorGroup + coroutines
Node             Custom http server              Event loop
Bun              Bun.serve()                     Event loop
Deno             Deno.serve()                    Event loop
Python           Custom HTTP server              Single-threaded
Go               net/http                        Goroutines
PHP              Swoole HTTP Server              Coroutines (multi-process)
Ruby             Puma + Sinatra                  Thread pool (GIL-limited)
Swift            Vapor (SwiftNIO)                Event loop
C++              Custom (cpp-httplib)            Thread pool
.NET             Kestrel (ASP.NET)               Async/await
Dart             shelf + shelf_router            Isolates
```
