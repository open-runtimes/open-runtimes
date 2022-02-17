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
  - [Architecture](#architecture)
    - [Load Balancer](#load-balancer)
    - [Executor](#executor)
    - [Adapter](#adapter)
    - [Runtime](#runtime)
    - [Function](#function)
    - [Build](#build)
  - [Structure](#structure)
  - [Testing](#testing)
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
| Node.js | 17.0    | [open-runtimes/node:17.0](https://hub.docker.com/r/open-runtimes/node) | [Node.js Examples](/runtimes/node-17.0/example) | [![Docker Pulls](https://img.shields.io/docker/pulls/open-runtimes/node?color=f02e65&style=flat-square)](https://hub.docker.com/r/open-runtimes/node) |
| Deno | 1.14    | [open-runtimes/deno:1.14](https://hub.docker.com/r/open-runtimes/deno) | [Deno Examples](/runtimes/deno-1.14/example) | [![Docker Pulls](https://img.shields.io/docker/pulls/open-runtimes/deno?color=f02e65&style=flat-square)](https://hub.docker.com/r/open-runtimes/deno) |
| Python | 3.10    | [open-runtimes/python:3.10](https://hub.docker.com/r/open-runtimes/python) | [Python Examples](/runtimes/python-3.10/example) | [![Docker Pulls](https://img.shields.io/docker/pulls/open-runtimes/python?color=f02e65&style=flat-square)](https://hub.docker.com/r/open-runtimes/python) |
| PHP | 8.0    | [open-runtimes/php:8.0](https://hub.docker.com/r/open-runtimes/php) | [PHP Examples](/runtimes/php-8.0/example) | [![Docker Pulls](https://img.shields.io/docker/pulls/open-runtimes/php?color=f02e65&style=flat-square)](https://hub.docker.com/r/open-runtimes/php) |

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

## Structure

All runtimes share a common basic structure, but each additionally adds runtime-specific files to properly support it's package manager.

```
.
├── build.sh
├── docker-compose.yml
├── Dockerfile
├── example
│   ├── (runtime-specific)
├── start.sh
├── README.md
└── server.X
```

| Name               	| Description                                                                                                                                           	|
|--------------------	|-------------------------------------------------------------------------------------------------------------------------------------------------------	|
| example/           	| Contains a sample function to demonstrate the usage of the runtime server                                                                             	|
| docker-compose.yml 	| Configuration to easily run the example code with `docker-compose up`                                                                                 	|
| Dockerfile         	| Instructions to build a runtime, install it's dependencies and setup the runtime server. These images are usually based on official alpine or ubuntu. 	|
| server.X           	| A HTTP server implemented in the respective runtime's language. File extension depends on your runtime. For instance, Python is `server.py`           	|
| build.sh           	| Script responsible for building user code. This can be package installations, or any specific build process the runtime requires.                     	|
| start.sh          	| Script to launch the HTTP server on port `3000`. Additionally, it also copies the user supplied code to a directory accessible to the server.         	|
| prepare.sh          | (optional) If a runtime requires preparation before building (for instance PHP), this file holds the logic for mapping the files.                     	|
| README.md          	| Runtime specific documentation                                                                                                                        	|

Every request sent to any of the runtimes must have the `X-Internal-Challenge` header. The value of this header has to match the value of environment variable `INTERNAL_RUNTIME_KEY` set on the runtime. All example scripts use `secret-key` as the key and we strongly recommend changing this key before production use.

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

## Testing

We use PHP framework PHPUnit to test Open Runtimes. Every PR is automatically tested by Travis CI, and tests run for all runtimes.

Before running the tests, make sure to install all required PHP libraries:

```bash
docker run --rm --interactive --tty --volume $PWD:/app composer install
```

Once ready, you can test runtimes. First, you need to pick which runtime you want to test. In this example you will be testing `node-17.0` runtime. You can test any runtime; make sure to get runtime name, php class name, and entrypoint from a runtime-related test in PHP file in `tests` directory.

To run tests, you execute `tests.sh` while providing information about runtime you want to test:

```bash
RUNTIME='node-17.0' PHP_CLASS='Node170' ENTRYPOINT='tests.js' sh tests.sh
```

## Contributing

All code contributions - including those of people having commit access - must go through a pull request and be approved by a core developer before being merged. This is to ensure a proper review of all the code.

We truly ❤️ pull requests! If you wish to help, you can learn more about how you can contribute to this project in the [contribution guide](CONTRIBUTING.md).

## Security

For security issues, kindly email us at [security@appwrite.io](mailto:security@appwrite.io) instead of posting a public issue on GitHub.

## Follow Us

Join our growing community around the world! See our official [Blog](https://medium.com/appwrite-io). Follow us on [Twitter](https://twitter.com/appwrite), [Facebook Page](https://www.facebook.com/appwrite.io), [Facebook Group](https://www.facebook.com/groups/appwrite.developers/) , [Dev Community](https://dev.to/appwrite) or join our live [Discord server](https://discord.gg/mkZcevnxuf) for more help, ideas, and discussions.

## License

This repository is available under the [MIT License](./LICENSE).