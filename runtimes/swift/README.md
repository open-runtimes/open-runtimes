# Swift Runtime
The runtime itself uses [Vapor](https://vapor.codes) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Runtimes introduction](https://github.com/open-runtimes/open-runtimes#runtimes-introduction) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `index.swift` file:

```bash
mkdir swift-function && cd swift-function
tee -a index.swift << END
func main(context: RuntimeContext) async throws -> RuntimeOutput {
    return try context.res.json(["n": Double.random(in:0...1)])
}

END

```

2. Build the code:
> Examples use Swift-5.9, but you can use any from `versions` directory.

```bash
docker run -e OPEN_RUNTIMES_ENTRYPOINT=index.swift --rm --interactive --tty --volume $PWD:/mnt/code openruntimes/swift:v4-5.9 sh helpers/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/swift:v4-5.9 sh helpers/start.sh "/usr/local/server/src/function/Runtime serve --env production --hostname 0.0.0.0 --port 3000"
```

4. In new terminal window, execute function:

```bash
curl -H "x-open-runtimes-secret: secret-key" -X GET http://localhost:3000/
```

Output `{ "n": 0.7232589496628183 }` with a random float will be displayed after the execution.

## Notes

- The `res` parameter has two methods:

    - `send()`: Send a string response to the client.
    - `json()`: Send a JSON response to the client.

You can respond with `json()` by providing object:

```swift
func main(req: RequestValue, res: RequestResponse) -> RequestResponse {
    res.json(data : [
        "message": "Hello Open Runtimes 👋",
        "variables": req.variables,
        "payload": req.payload,
        "headers": req.headers
    ])
}
```

- To handle dependencies, you need to have `Package.swift` file. Dependencies will be automatically installed.

- The default entrypoint is `Sources/index.swift`. If your entrypoint differs, make sure to provide it in the JSON body of the request: `{ "file": "app.swift" }`.

- Swift does not require `INTERNAL_RUNTIME_ENTRYPOINT` like other runtimes. In Swift, you only need to make sure a function called `main` is defined in any of your `.swift` files.

- Swift runtime currently doesn't support ARM, because there are no official ARM images.

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
