# Kotlin Runtime 1.9

The runtime itself uses [Javalin](https://github.com/tipsy/javalin) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `Index.kt` file:

```bash
mkdir kotlin-function && cd kotlin-function
tee -a Index.kt << END
package io.openruntimes.kotlin

public class Index {
    fun main(context: RuntimeContext): RuntimeOutput = context.res.json(mutableMapOf("n" to Math.random()))
}

END

```

2. Build the code:
> Examples use Kotlin-1.9, but you can use any from `versions` directory.

```bash
docker run -e OPEN_RUNTIMES_ENTRYPOINT=Index.kt --rm --interactive --tty --volume $PWD:/mnt/code openruntimes/kotlin:v4-1.9 sh helpers/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/kotlin:v4-1.9 sh helpers/start.sh "java -jar /usr/local/server/src/function/kotlin-runtime-1.0.0.jar"
```

4. In new terminal window, execute function:

```bash
curl -H "x-open-runtimes-secret: secret-key" -X GET http://localhost:3000/
```

Output `{"n":0.7232589496628183}` with random float will be displayed after the execution.

## Notes

- When writing function for this runtime, ensure it is named `main`. An example of this is:

```kotlin
package io.openruntimes.kotlin

public class Index {
    @Throws(Exception::class)
    fun main(context: RuntimeContext): RuntimeOutput {
        return context.res.send("Hello Open Runtimes 👋");
    }
}
```

- Ensure your Kotlin files starts with `package io.openruntimes.kotlin;`

- To handle dependencies, you need to have any `.gradle` file including a `dependencies` block. Dependencies will be automatically installed.

- The default entrypoint is `Index.kt`. If your entrypoint differs, make sure to configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable during build, for instance, `OPEN_RUNTIMES_ENTRYPOINT=src/App.kt`.

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
