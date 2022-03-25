# Kotlin Runtime 1.6

This is the Open Runtime that builds and runs Kotlin code based on a `openjdk:17-jdk-slim` base image.

The runtime itself uses [Javalin](https://github.com/tipsy/javalin) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `Index.kt` file:

```bash
mkdir kotlin-or && cd kotlin-or
printf "fun main(req: RuntimeRequest, res: RuntimeResponse) = res.json(mapOf(\"n\" to Math.random()))" > Index.kt
```

2. Build the code:

```bash
docker run -e INTERNAL_RUNTIME_ENTRYPOINT=Index.kt --rm --interactive --tty --volume $PWD:/usr/code openruntimes/kotlin:1.6 sh /usr/local/src/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e INTERNAL_RUNTIME_KEY=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/tmp/code.tar.gz:ro openruntimes/kotlin:1.6 sh /usr/local/src/start.sh
```

4. In new terminal window, execute function:

```bash
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload": "{}"}'
```

Output `{"n":0.7232589496628183}` with random float will be displayed after the execution.

## Local development

1. Clone the [open-runtimes](https://github.com/open-runtimes/open-runtimes) repository:

```bash
git clone https://github.com/open-runtimes/open-runtimes.git
```

2. Enter the kotlin runtime folder:

```bash
cd open-runtimes/runtimes/kotlin-1.6
```

3. Run the included example cloud function:

```bash
docker-compose up -d
```

4. Execute the function:

```bash
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload": "{}"}'
```

You can now send `POST` request to `http://localhost:3000`. Make sure you have header `x-internal-challenge: secret-key`. If your function expects any parameters, you can pass an optional JSON body like so: `{ "payload":"{}" }`.

You can also make changes to the example code and apply the changes with the `docker-compose restart` command.

## Notes

- The `res` parameter has two methods:

    - `send()`: Send a string response to the client.
    - `json()`: Send a JSON response to the client.

You can respond with `json()` by providing object:

```kotlin
@Throws(Exception::class)
fun main(req: RuntimeRequest, res: RuntimeResponse): RuntimeResponse = res.json(mapOf(
    "message" to "Hello Open Runtimes 👋",
    "env" to req.env,
    "headers" to req.headers,
    "payload" to req.payload
))
```

- To handle dependencies, you need to have any `.gradle` file including a `dependencies` block. Dependencies will be automatically cached and installed, so you don't need to include the `build` folder in your function.

- The default entrypoint is `Index.kotlin`. If your entrypoint differs, make sure to configure it using `INTERNAL_RUNTIME_ENTRYPOINT` environment variable, for instance, `INTERNAL_RUNTIME_ENTRYPOINT=src/App.kotlin`.

## Authors

**Eldad Fux**

+ [https://twitter.com/eldadfux](https://twitter.com/eldadfux)
+ [https://github.com/eldadfux](https://github.com/eldadfux)

**Jake Barnby**

+ [https://github.com/abnegate](https://github.com/abnegate)

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
