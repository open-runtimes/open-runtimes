# Java Runtime 18.0

This is the Open Runtime that builds and runs Java code based on a `openjdk:18-jdk-slim` base image. 

The runtime itself uses [Rapidoid](https://github.com/rapidoid/rapidoid) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `Index.java` file:

```bash
mkdir java-function && cd java-function
tee -a Index.java << END
package io.openruntimes.java;
import java.util.Collections;

public class Index {
    public RuntimeOutput main(RuntimeContext context) throws Exception {
        return context.getRes().json(Collections.singletonMap("n", Math.random()));
    }
}

END

```

2. Build the code:

```bash
docker run -e OPEN_RUNTIMES_ENTRYPOINT=Index.java --rm --interactive --tty --volume $PWD:/mnt/code openruntimes/java:v3-18.0 sh helpers/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/java:v3-18.0 sh helpers/start.sh "java -jar /usr/local/server/src/function/java-runtime-1.0.0.jar"
```

4. In new terminal window, execute function:

```bash
curl -H "x-open-runtimes-secret: secret-key" -X GET http://localhost:3000/
```

Output `{"n":0.7232589496628183}` with random float will be displayed after the execution.

## Local development

1. Clone the [open-runtimes](https://github.com/open-runtimes/open-runtimes) repository:

```bash
git clone https://github.com/open-runtimes/open-runtimes.git
```

2. Enter the Java runtime folder:

```bash
cd open-runtimes/runtimes/java-18.0
```

3. Run the included example cloud function:

```bash
docker compose up -d
```

4. Execute the function:

```bash
curl -H "x-open-runtimes-secret: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"id": "4"}'
```

You can now send `POST` request to `http://localhost:3000`. Make sure you have header `x-open-runtimes-secret: secret-key`.

You can also make changes to the example code and apply the changes with the `docker compose restart` command.

## Notes

- When writing function for this runtime, ensure it is named `main`. An example of this is:

```java
package io.openruntimes.java;

public class Index {
    public RuntimeOutput main(RuntimeContext context) throws Exception {
        return context.res.send("Hello Open Runtimes 👋");
    }
}
```

- Ensure your Java files starts with `package io.openruntimes.java;`

- To handle dependencies, you need to have any `.gradle` file including a `dependencies` block. Dependencies will be automatically cached and installed, so you don't need to include the `build` folder in your function.

- The default entrypoint is `Index.java`. If your entrypoint differs, make sure to configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable during build, for instance, `OPEN_RUNTIMES_ENTRYPOINT=src/App.java`.

## Authors

**Eldad Fux**

+ [https://twitter.com/eldadfux](https://twitter.com/eldadfux)
+ [https://github.com/eldadfux](https://github.com/eldadfux)

**Jake Barnby**

+ [https://github.com/abnegate](https://github.com/abnegate)

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
