# Open Runtimes ⚡️

![open-runtimes-box-bg-cover](https://user-images.githubusercontent.com/1297371/151676246-0e18f694-dfd7-4bab-b64b-f590fec76ef1.png)

---

[![Discord](https://img.shields.io/discord/937092945713172480?label=discord&style=flat-square)](https://discord.gg/mkZcevnxuf)
[![Build Status](https://img.shields.io/travis/com/appwrite/appwrite?style=flat-square)](https://travis-ci.com/appwrite/appwrite)
[![Twitter Account](https://img.shields.io/twitter/follow/appwrite?color=00acee&label=twitter&style=flat-square)](https://twitter.com/appwrite)

<!-- [![Docker Pulls](https://img.shields.io/docker/pulls/appwrite/appwrite?color=f02e65&style=flat-square)](https://hub.docker.com/r/appwrite/appwrite) -->

Runtime environments for serverless cloud computing for multiple coding languages, aiming to create a consistent and predictable open standard for writing cloud functions in containerized systems.

- [Open Runtimes ⚡️](#open-runtimes-️)
  - [Features](#features)
  - [Roadmap](#roadmap)
  - [Images](#images)
  - [Runtimes introduction](#runtimes-introduction)
  - [Architecture](#architecture)
    - [Load Balancer](#load-balancer)
    - [Executor](#executor)
    - [Adapter](#adapter)
    - [Runtime](#runtime)
    - [Function](#function)
    - [Build](#build)
  - [Contributing](#contributing)
  - [Security](#security)
  - [Follow Us](#follow-us)
  - [License](#license)

## Features

* **Flexibility** (WIP) - Designed to work with multiple orchestrators using different adapters. For now only Docker and Swarm are supported, Kubernetes support is planned.
* **Performance** - Cold starts in less than ~100ms warm stars has added latency of less than 1ms.
* **Wide Support** - Support over 10 different coding languages and over 20 different runtimes. Adding new ones is straight forward.
* **Open Source** - Released under the MIT license, free to use and extend.
* **Ecosystem** - Fast growing ecosystem of ready-to-use functions for easy reuse on different platforms.

## Roadmap

* Kubernetes Adapter - Native cloud support and easy deployment
* Official CLI - Easy deployments
* Catalog - Browse the open-runtimes functions ecosystem
* Autoscaling - Automated scaling features based on hosts and runtime metrics

## Images

| Name    | Version | Docker Hub                 | Examples | Pulls |
|---------|---------|----------------------------|----------|-------|
| Node.js | 15.0    | [open-runtimes/node.js:15.0](https://hub.docker.com/r/open-runtimes/node.js) | [Node.js Examples]() | [![Docker Pulls](https://img.shields.io/docker/pulls/open-runtimes/node.js?color=f02e65&style=flat-square)](https://hub.docker.com/r/open-runtimes/node.js) |
| Node.js | 16.0    | [open-runtimes/node.js:16.0](https://hub.docker.com/r/open-runtimes/node.js) | [Node.js Examples]() | [![Docker Pulls](https://img.shields.io/docker/pulls/open-runtimes/node.js?color=f02e65&style=flat-square)](https://hub.docker.com/r/open-runtimes/node.js) |
| PHP     | 8.0     | [open-runtimes/php:8.0](https://hub.docker.com/r/open-runtimes/php)      | [PHP Examples]() | [![Docker Pulls](https://img.shields.io/docker/pulls/open-runtimes/php?color=f02e65&style=flat-square)](https://hub.docker.com/r/open-runtimes/php) |

## Runtimes introduction

All runtimes share a common basic structure, but each additionally adds runtime-specific files to properly support it's package manager.

```
.
├── build.sh
├── docker-compose.yml
├── Dockerfile
├── example
│   ├── (runtime-specific)
├── deploy.sh
├── launch.sh
├── README.md
└── server.X
```

- `example` directory includes example script using language of the specific runtime. File `docker-compose.yml` is configuration to easily run example code with `docker-compose up`.
- `Dockerfile` file holds an image definition that prepares environment for the runtime to run on. These images are usually based on Linux Alpine or Linux Ubuntu.
- `server.X` file is a runtime-specific HTTP server implementation. File extention is different for every programming language, for instance, Python is `server.py`.
- `build.sh` script builds source code into executable script. This ca be either package installations, or build process of the runtime.
- `launch.sh` script take executable script and prepare server that will execute it by running HTTP server on port `3000`
- `deploy.sh` script is all-in-one solution that builds a script and launches HTTP server. This script is really handy for simple usage without executor.
- `README.md` is runtime documentation.

Every request sent to any of the runtimes must have header `X-Internal-Challenge`. The value of this header has to match the value of environment variable `INTERNAL_RUNTIME_KEY` set on the runtime. All example scripts use `password` as the key and we strongly recommend adjusting this key before production use.

All requests should also have JSON body with the following structure:

```json5
{
    // Directory where the code is placed
    "path": "/usr/code",
    // Script entrypoint
    "file": "index.js",

    // Following will be exposed to the function
    "env": {
        // Environment varialbes
    },
    "payload": {
        // Execution data
    },
    "headers": {
        // Request headers
    }
}
```

All body parameters are optional. The values used in the example above are the default values.

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

## Contributing

All code contributions - including those of people having commit access - must go through a pull request and be approved by a core developer before being merged. This is to ensure a proper review of all the code.

We truly ❤️ pull requests! If you wish to help, you can learn more about how you can contribute to this project in the [contribution guide](CONTRIBUTING.md).

## Security

For security issues, kindly email us at [security@appwrite.io](mailto:security@appwrite.io) instead of posting a public issue on GitHub.

## Follow Us

Join our growing community around the world! See our official [Blog](https://medium.com/appwrite-io). Follow us on [Twitter](https://twitter.com/appwrite), [Facebook Page](https://www.facebook.com/appwrite.io), [Facebook Group](https://www.facebook.com/groups/appwrite.developers/) , [Dev Community](https://dev.to/appwrite) or join our live [Discord server](https://discord.gg/mkZcevnxuf) for more help, ideas, and discussions.

## License

This repository is available under the [MIT License](./LICENSE).