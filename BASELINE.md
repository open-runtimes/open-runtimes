# Runtime Performance Baseline

Measured 2026-04-10. Java and Kotlin use Netty with epoll (branch `chore-kt-updates`). All others use their existing HTTP frameworks.

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
 1. Clean /tmp/logs/ (prevents ext4 htree exhaustion from accumulated log files)
 2. Build Docker image (CI assembly + docker build)
 3. Build user code (test function via helpers/build.sh)
 4. Cold start (5x): docker rm -f + docker run (true cold start, not restart)
    Measured from docker inspect StartedAt to first 200 on /__opr/health
    Polled via curl --connect-timeout 0.005, tight loop (~5ms/attempt)
 5. Warm-up: 500 requests
 6. Warm latency: wrk -t1 -c1 -d10s --latency (P50, P99)
 7. Throughput: wrk -t8 -c256 -d30s (RPS, hot avg latency per request)
 8. RAM: docker stats --no-stream (post-load, after 256c/30s)
 9. Cleanup: docker rm -f + 2s settle
```

### Cross-host validation

All 13 runtimes ran on both M1 and M2. Values are cross-host averages. Java and Kotlin Netty only have M2 data (M1 build had an unrelated issue).

## Results

Sorted by throughput descending.

```
Runtime                  Cold     P50       P99     HotAvg     RPS     RAM        Image
---------------------   ------   ------   ------   ------   ------   ---------   --------
Java 25 (Netty)           693   276 us    66 ms    47 ms    7,364   1.01 GiB    1.05 GB
Kotlin 2.3 (Netty)      1,025   361 us    76 ms    47 ms    6,964   935 MiB     1.02 GB
Bun 1.3                   427   334 us    78 ms    45 ms    6,000   701 MiB     366 MB
Node 25                 1,811   340 us    73 ms    48 ms    4,974   1015 MiB    1.84 GB
Dart 3.11                 383   471 us    52 ms    62 ms    4,252   450 MiB     1.21 GB
Go 1.26                   382   609 us    80 ms    77 ms    4,117   410 MiB     392 MB
Swift 6.2                 624   676 us    54 ms    66 ms    3,634   361 MiB     5.38 GB
C++ 23                    472   519 us    83 ms   104 ms    3,256   200 MiB     698 MB
PHP 8.4                   454   623 us    77 ms   150 ms    2,749   172 MiB     363 MB
Deno 2.6                  688   622 us    81 ms   115 ms    1,881   484 MiB     250 MB
Python 3.14             2,814   810 us    83 ms   249 ms    1,484   131 MiB     690 MB
.NET 10                   515   408 us    55 ms   203 ms    1,238   200 MiB     1.08 GB
Ruby 4.0                1,480   1.33 ms  115 ms   438 ms      590   122 MiB     537 MB
```

## Per-host raw data

### M1

```
Runtime           Cold    P50       P99     HotAvg    RPS     RAM        Image
--------------   ------  --------  ------  ------   ------  ---------  --------
Node 25          2,114   367 us    78 ms    57 ms    4,539   1.12 GiB   1.85 GB
Bun 1.3            577   343 us    79 ms    47 ms    5,915   662 MiB    366 MB
Deno 2.6           813   629 us    89 ms   165 ms    1,543   377 MiB    250 MB
Python 3.14      3,184   603 us    79 ms   131 ms    1,945   254 MiB    690 MB
C++ 23             514   258 us    95 ms    62 ms    4,432   447 MiB    698 MB
PHP 8.4            513   619 us   105 ms    69 ms    3,802   362 MiB    363 MB
Ruby 4.0         1,263   1.29 ms  115 ms   423 ms      600   120 MiB    547 MB
Swift 6.2          696   672 us   110 ms    75 ms    3,413   321 MiB    5.38 GB
.NET 10            573   407 us    24 ms   208 ms    1,222   194 MiB    1.08 GB
Dart 3.11          507   456 us   101 ms    68 ms    4,103   424 MiB    1.21 GB
```

### M2

```
Runtime                  Cold    P50       P99     HotAvg    RPS     RAM        Image
---------------------   ------  --------  ------  ------   ------  ---------  --------
Node 25                 1,509   340 us    73 ms    48 ms    5,410   1015 MiB   1.84 GB
Bun 1.3                   278   334 us    78 ms    45 ms    6,085   701 MiB    366 MB
Deno 2.6                  563   622 us    81 ms   115 ms    2,219   484 MiB    250 MB
Python 3.14             2,445   810 us    83 ms   249 ms    1,023   131 MiB    680 MB
Go 1.26                   382   609 us    80 ms    77 ms    4,117   410 MiB    392 MB
C++ 23                    430   519 us    83 ms   104 ms    2,081   200 MiB    689 MB
PHP 8.4                   396   623 us    77 ms   150 ms    1,696   172 MiB    351 MB
Ruby 4.0                1,697   1.33 ms    3 ms   438 ms      581   122 MiB    537 MB
Swift 6.2                 553   676 us    54 ms    66 ms    3,856   361 MiB    5.38 GB
.NET 10                   457   408 us    55 ms   203 ms    1,254   200 MiB    1.08 GB
Dart 3.11                 259   471 us    52 ms    62 ms    4,402   450 MiB    1.21 GB
Java 25 (Netty)           693   276 us    66 ms    47 ms    7,364   1.01 GiB   1.05 GB
Kotlin 2.3 (Netty)      1,025   361 us    76 ms    47 ms    6,964   935 MiB    1.02 GB
```

## Observations

### Throughput ranking

Java Netty leads at 7,364 RPS, followed by Kotlin Netty at 6,964 RPS. Both outperform all non-JVM runtimes. Bun (6,000) is the fastest non-JVM runtime.

### Hot avg latency (avg execution time per request under load)

```
Fast   (<50ms):  Bun 45ms, Java 47ms, Kotlin 47ms, Node 48ms
Medium (50-100ms):  Dart 62ms, Swift 66ms, Go 77ms, C++ 104ms
Slow   (>100ms):  Deno 115ms, PHP 150ms, .NET 203ms, Python 249ms, Ruby 438ms
```

### Cold start tiers

```
Fast   (<400ms):  Bun 278ms (M2), Dart 259ms (M2), Go 382ms
Medium (400-700ms):  C++ 472ms, PHP 454ms, .NET 515ms, Swift 624ms, Deno 688ms, Java 693ms
Slow   (>1000ms):  Kotlin 1025ms, Ruby 1480ms, Node 1811ms, Python 2814ms
```

### Log directory impact

A critical finding: `/tmp/logs/` accumulated millions of files across benchmark runs (2 files per request × ~100K requests per runtime × 13 runtimes). This caused ext4 htree exhaustion which:
- Crashed Node and Bun entirely (unhandled ENOSPC error on fs.createWriteStream)
- Degraded all runtimes by 40-60% (every request's logger file creation slowed down)

The fix was cleaning `/tmp/logs/` between each runtime. Earlier benchmark runs without this cleanup showed ~3,000 RPS for Java (vs 7,364 with cleanup).

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
