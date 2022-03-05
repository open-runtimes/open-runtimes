# Creating a new functions runtime 🏃

This document is part of the Open Runtimes contributors' guide. Before you continue reading this document make sure you have read the [Code of Conduct](https://github.com/open-runtimes/open-runtimes/blob/main/CODE_OF_CONDUCT.md) and the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md).

## 1. Prerequisites
For a runtime to work, two prerequisites **must** be met due to the way Open Runtimes's Runtime Execution Model works:

 - [ ] The Language in question must be able to run a web server that can serve JSON and text.
 - [ ] The Runtime must be able to be packaged into a Docker container
 
 Note: Both Compiled and Interpreted languages work with Open Runtimes's execution model but are written in slightly different ways.

It's really easy to contribute to an open-source project, but when using GitHub, there are a few steps we need to follow. This section will take you step-by-step through the process of preparing your local version of Open Runtimes, where you can make any changes without affecting Open Runtimes right away.

> If you are experienced with GitHub or have made a pull request before, you can skip to [Implement New Runtime](#2-implement-new-runtime).

### 1.1 Fork the Open Runtimes repository

Before making any changes, you will need to fork Open Runtimes's repository to keep branches on the official repo clean. To do that, visit [Open Runtimes's Runtime repository](https://github.com/open-runtimes/open-runtimes) and click on the fork button.

[![Fork button](https://github.com/open-runtimes/open-runtimes/raw/main/docs/images/fork.png)](https://github.com/open-runtimes/open-runtimes/raw/main/docs/images/fork.png)

This will redirect you from `github.com/open-runtimes/open-runtimes` to `github.com/YOUR_USERNAME/open-runtimes`, meaning all changes you do are only done inside your repository. Once you are there, click the highlighted `Code` button, copy the URL and clone the repository to your computer using the `git clone` command:

```bash
$ git clone COPIED_URL
```

> To fork a repository, you will need a basic understanding of CLI and git-cli binaries installed. If you are a beginner, we recommend you to use `Github Desktop`. It is a clean and simple visual Git client.

Finally, you will need to create a `feat-XXX-YYY-runtime` branch from the `main` branch and switch to it. The `XXX` should represent the issue ID and `YYY` the runtime name.

## 2. Implement new runtime

### 2.1 Preparing the files for your new runtime

The first step to writing a new runtime is to create a folder within `/runtimes` with the name of the runtime and the version separated by a dash. For instance, if I was to write a Rust Runtime with version 1.55 the folder name would be: `rust-1.55`.

Within that folder you will need to create a few basic files that all Open Runtimes runtimes require:


- `Dockerfile` - Dockerfile that explains how the container will be built.
- `README.md` - A readme file explaining the runtime and any special notes for the runtime. A good example of this is the PHP 8.0 runtime.

### 2.2 Differences between compiled and interpreted runtimes

Runtimes within Open Runtimes are created differently depending on whether they are compiled or interpreted. This is due to the fundamental differences between the two ways of running the code.

Interpreted languages have both a `build.sh` file and a `start.sh` file.

The `build.sh` file for an interpreted runtime is normally used for installing any dependencies for both the server itself and the user's code and then to copy it to the `/usr/code` folder which is then packaged and can be used later for running the server. The build script is always executed during the build stage of a function deployment.

The `start.sh` file for an interpreted runtime should extract the `/tmp/code.tar.gz` file that contains both the user's code and the dependencies. This tarball was created by Open Runtimes build stage, and start process should install the dependencies that were pre-downloaded by the build stage. This is most likely done by moving them into the relevant locations for that runtime, most likely near server code. It will then run the server ready for execution.

---

The `build.sh` script for a compiled runtime is used to move the user's source code and rename it into source files for the runtime (The `INTERNAL_RUNTIME_ENTRYPOINT` environment variable can help with this). It will also build the code and move it into the `/usr/code` folder.

#### Note:

- `/tmp/code.tar.gz` is always created from the `/usr/code` folder as an output of the build stage. If you need any files for either compiled or interpreted runtimes you should place them there and extract them from the `/tmp/code.tar.gz` during the `start.sh` script to get the files you need.

- If you need to do any changes to user files during the build, we recommend copying them into `/usr/builds`. This way you do changes internally, and don't actually touch the code on the harddrive of the user.

- If your runtime needs any additional transformation of user input, we recommend doing that in `prepare.sh`, and running this preparation script from `build.sh`. This helps us differenciate between build process, and process of adjusting user code.

- Make sure to checkout existing runtimes to also include files such as `.env`, `.gitignore` and so on.

- Once done with runtime, please make sure to include `example` folder with example script. This script must use HTTP client library, and it must send a specific request to read specific API. We do this to make sure our example covers most of the usage, and you can easily see exact same script in any runtime. Please refer to other runtimes to see what exactly needs to be done in the example script.

### 2.3 Writing the runtime

Internally, the runtime can be anything you like as long as it follows the standards set by the other runtimes.

The best way to go about writing a runtime is like so:

Initialize a web server that runs on port 3000 and binds to the 0.0.0.0 IP and on each `POST` request do the following:

1. Check that the `x-internal-challenge` header matches the `INTERNAL_RUNTIME_KEY` environment variable. If not return an error with a `401` status code and an `unauthorized` error message.

2. Decode the executor's JSON POST request. This normally looks like so:
```json
{
 "env": {
    "USER_KEY":"abcd1234"
 },
 "headers": {
    "hello":"world!"
 },
 "payload":"An Example Payload"
}
```

You must create two classes for users to use within their scripts.

A `Request` Class and a `Response` class. The `Request` class must store `env`, `payload` and `headers` and pass them to the user's function. The Request always goes before the response in the user's function parameters.

The `Response` class must have two functions:

- `send(string)` function which will return text to the request
- `json(object)` function which will return JSON to the request setting the appropriate headers

For languages that have dynamic typing such as JS, you can pass an object with these attributes if you like.

Please make sure to add appropriate checks to make sure the imported file is a function that you can execute.

3. Finally, execute the function and handle whatever response the user's code returns. Wrap the function into a `try catch` statement to handle any errors the user's function encounters and return them cleanly to the executor with the error schema.

### 2.4 The Error Schema

All errors that occur during the execution of a user's function **MUST** be returned using this JSON Object otherwise OpenRuntimes will be unable to parse them for the user.

```json5
{
    "code": 500, // (Int) Use 404 if function not found or use 401 if the x-internal-challenge check failed.
    "message": "Error: Tried to divide by 0 \n /usr/code/index.js:80:7", // (String) Try to return a stacktrace and detailed error message if possible. This is shown to the user.
}
```

### 2.5 Writing your Dockerfile

The Dockerfile is very important as it's the environment you are creating to run build the runtime and also run the code if you are writing an interpreted runtime (compiled runtimes will use an `alpine` or `ubuntu` image to run the compiled executable).

The first thing you need to do is find a Docker image to base your runtime off. You can find these at [Docker Hub](https://hub.docker.com). If possible, try to use verified official builds of the language you are creating a runtime for. We also prefer `alpine` based images, as they help save a lot of space.

Next, in your Dockerfile at the start, add the Docker image you want to base it off at the top like so:

```bash
FROM Dart:2.12 # Dart is used as an example.
```

This will download and require the image when you build your runtime and allow you to use the toolset of the language you are building a runtime for.

Create the folders you will use in your build step:

```bash
RUN mkdir -p /usr/local/src/
RUN mkdir -p /usr/code
RUN mkdir -p /usr/workspace
RUN mkdir -p /usr/builds
```

Next copy your source code and set the working directory for the image like so:

```bash
WORKDIR /usr/local/src
COPY . .
```

Next, you want to make sure you are adding execute permissions to any scripts you may run, the main ones are `build.sh` and `start.sh`. You can run commands in Dockerfile's using the `RUN` prefix like so:

```bash
RUN chmod +x ./build.sh
RUN chmod +x ./start.sh
```

If needed use the `RUN` commands to install any dependencies you require for the build stage.

Then you mark port 3000 as exposed, since this is the port runtime's HTTP server will be running on:

```bash
EXPOSE 3000
```

Finally, you'll add a `CMD` command. this should be:

```bash
CMD ["/usr/local/src/start.sh"]
```

Since this will use your launch script when the runtime starts.

## 3. Building your Docker image and adding it to the list

With your runtime successfully created, you can now move on to building your Docker image and adding it to the script files used for generating all of the image files.

Open up the `./build.sh` script at the root of the project and add your runtime to it. The following is an example with dart version 2.12:

```bash
echo 'Dart 2.12...'
docker build -t openruntimes/dart:2.12 ./runtimes/dart-2.12
```

## 4. Adding tests

### 4.1 Creating your test script
Create a new folder in `./tests` and name it the same name as the folder where you placed your runtime code. For example, if you are creating a runtime for dart 2.12 you would name it `dart-2.12`.

Next create a new PHP file in the `./tests` folder and name it by your language followed by it's version with **no dots and no spaces**. For example, if you are creating a runtime for dart 2.12 you would name it `Dart212.php`. Within this PHP file you will place the following code:

```php
<?php

namespace Tests;

// Runtime: {{runtime name}}
// PHP class: {{ file name }}
// Entrypoint: {{ entrypoint name}}

class {{ file name }} extends Base
{
}
```

Note: Make sure to replace `{{runtime name}}` with the name of your runtime for example: `dart-2.12` and the `{{file name}}` with the same name as the file you are currently working in without `.php` so for example `Dart212`. Also make sure to replace `{{entrypoint name}}` with entrypoint to where your test file will be at, including extention, for instance `lib/tests.dart`.

Next, go back into the folder you created earlier in `./tests/` and create a new source file for your language called `test` with the extension of the language you are adding. This must be exactly same as entrypoint set in the PHP file comment. For example, if you are creating a runtime for dart 2.12 you would name it `lib/test.dart`.

Within the folder you will need to create a function for your runtime that will do the following:

1. Decode the payload as JSON

2. Set a variable called `id` to the value of the `id` key in the payload or to `1` if it doesn't exist. This `id` will be of type string.

3. Fetch `https://jsonplaceholder.typicode.com/todos/$id` using a HTTP Client that you got from your language's package manager (This is to test your dependency installation stage is working). Make sure to replace `$id` with the ID provided from payload.

4. Return res.json with the following Schema:

```json5
    "isTest": true,
    "message": "Hello Open Runtimes 👋",
    "header": req.headers['x-test-header'],
    "env": req.env['test-env'],
    "todo": {{body from your todo API http response}},
```

### 4.2 Adding your runtime to Travis

Edit the `.travis.yml` file and add your runtime to the `env` section of it like so:

```yaml
  # {{Language Name}}
  - RUNTIME={{full runtime name with version, e.g. dart-2.12}}
    PHP_CLASS={{Name of the PHP Class you made earlier, e.g. Dart212}}
    ENTRYPOINT={{Name of your entrypoint file, e.g. test.dart}}
    SERVER_PROCESS="{{The name of the process that will be launched in your container, e.g. runtime}}"
    IMAGE={{Full image name including the openruntime/ prefix, e.g. openruntimes/dart-2.12}}
    ARCH={{List of architecture supported by this runtime seperated by commas, e.g. linux/amd64,linux/arm64}}
```

You will have to create multiple of these for each version of the language you are adding. Please don't add too many, and for initial pull reuqest, we highly recommend only adding one version, as it will be easier to review and update.

### 4.3 Running the tests

To run your tests locally, go ahead and run the following command in your terminal:

```bash
RUNTIME={{Your Runtime}} ENTRYPOINT={{ your entrypoint }} SERVER_PROCESS={{ your process }} PHP_CLASS={{ your class }} ./tests.sh 
```

Replace the curly brackets with the values you set in `.travis.yml` and make sure to run the command in the root of the repository.

If all tests pass then move on to the next step, otherwise you will need to troubleshoot the problem before continuing.

## 5. Raise a pull request

First of all, commit the changes with the message `Added XXX Runtime` and push it. This will publish a new branch to your forked version of Open Runtimes. If you visit it at `github.com/YOUR_USERNAME/php-runtimes`, you will see a new alert saying you are ready to submit a pull request. Follow the steps GitHub provides, and at the end, you will have your pull request submitted.

## 🤕 Stuck ?

If you need any help with the contribution, feel free to head over to our [Discord channel](https://discord.gg/fP6W2qEzfQ) and we'll be happy to help you out.
