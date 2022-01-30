# Open Runtimes

![open-runtimes-box-bg-cover](https://user-images.githubusercontent.com/1297371/151676246-0e18f694-dfd7-4bab-b64b-f590fec76ef1.png)

---

[![Discord](https://img.shields.io/discord/937092945713172480?label=discord&style=flat-square)](https://discord.gg/46Bq3VZX)
[![Build Status](https://img.shields.io/travis/com/appwrite/appwrite?style=flat-square)](https://travis-ci.com/appwrite/appwrite)
[![Twitter Account](https://img.shields.io/twitter/follow/appwrite?color=00acee&label=twitter&style=flat-square)](https://twitter.com/appwrite)

<!-- [![Docker Pulls](https://img.shields.io/docker/pulls/appwrite/appwrite?color=f02e65&style=flat-square)](https://hub.docker.com/r/appwrite/appwrite) -->

Runtime environments for serverless cloud computing for multiple coding languages, aiming to create a consistent and predictable open standard for writing cloud functions in containerized systems.

## Images

| Name    | Version | Docker Hub                 | Pulls |
|---------|---------|----------------------------|-------|
| Node.js | 15.0    | [open-runtimes/node.js:15.0](https://hub.docker.com/r/open-runtimes/node.js) | [![Docker Pulls](https://img.shields.io/docker/pulls/open-runtimes/node.js?color=f02e65&style=flat-square)](https://hub.docker.com/r/open-runtimes/node.js) |
| Node.js | 16.0    | [open-runtimes/node.js:16.0](https://hub.docker.com/r/open-runtimes/node.js) | [![Docker Pulls](https://img.shields.io/docker/pulls/open-runtimes/node.js?color=f02e65&style=flat-square)](https://hub.docker.com/r/open-runtimes/node.js) |
| PHP     | 8.0     | [open-runtimes/php:8.0](https://hub.docker.com/r/open-runtimes/php)      | [![Docker Pulls](https://img.shields.io/docker/pulls/open-runtimes/php?color=f02e65&style=flat-square)](https://hub.docker.com/r/open-runtimes/php) |


## Architecture

![Architecture](architecture.v2.drawio.svg)

### Load Balancer

The Load Balancer get requests for endpoints and responsible for balancing and scaling the requests between multiple hosts where runtime executors are available. This is the layer where you want to implement auto-scaling and keep track of which host has which runtimes available to allow wise spending of computing resources.

### Executor

The Executor is responsible for starting runtimes (AKA cold starts), and directing requests, environment variables, and user inputs to each runtime. In addition, the Executor will also be responsible for managing timeouts, max runtime allowed in parallel, and cleanup of inactive runtimes in the chosen interval.

### Adapter

The Adapter is a software layer component that interacts with the container orchestration engine to manage the compute runtimes.

### Runtime

The Runtime is a containerized isolated environment to run user-provided code. The runtime is spinning an HTTP TCP server on startup from one of the supported languages and handles requests on demand. Multiple runtimes of the same function can potentially run on the same or multiple hosts.

### Function

The Functions is a user provider packaged of code that is mounted to each Runtime and is executed inside the isolated environment. The package code should already be compiled and provided with all required dependencies.

### Build

The Build is composed from a queue and set of workers, the build process receives the raw codebase from the filesystem or a VCS and compiles or packages it with all dependencies. The build help with providing the dev's Function as a ready-to-execute codebase for the Runtime.