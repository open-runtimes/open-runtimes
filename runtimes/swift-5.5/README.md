# Swift Runtime 5.5

This is the Open Runtime that builds and runs NodeJS code based on a `swiftarm/5.5.2-focal-multi-arch` base image. 

The runtime itself uses [Vapor](https://vapor.codes) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Runtimes introduction](https://github.com/open-runtimes/open-runtimes#runtimes-introduction) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `index.swift` file:

```bash
mkdir swift-test && cd swift-test
echo "\
import Foundation\
\
func main(req: RequestValue, res: RequestResponse) -> RequestResponse {\
    return res.json(data: [\"Hello": "World!\"])\
}\
" > index.swift
```

2. Build the code:

```bash
docker run --rm --interactive --tty --volume $PWD:/usr/code open-runtimes/swift:5.5 sh /usr/local/src/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e INTERNAL_RUNTIME_KEY=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/tmp/code.tar.gz:ro open-runtimes/swift:5.5 sh /usr/local/src/start.sh
```

4. In new terminal window, execute function:

```
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload": "{}"}'
```

Output `{ "n": 0.7232589496628183 }` with a random float will be displayed after the execution.

## Local development

1. Clone the [open-runtimes](https://github.com/open-runtimes/open-runtimes) repository:

```bash
git clone https://github.com/open-runtimes/open-runtimes.git
```

2. Enter the Swift runtime folder:

```bash
cd open-runtimes/runtimes/swift-5.5
```

3. Run the included example cloud function:

```bash
docker-compose up -d
```

4. Execute the function:

```bash
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload": "{}"}'
```

You can now send `POST` request to `http://localhost:3000`. Make sure you have header `x-internal-challenge: secret-key`. If your function expects any parameters, you can pass an optional JSON body like so: `{ "payload":{} }`.

You can also make changes to the example code and apply the changes with the `docker-compose restart` command.

## Notes

- The `res` parameter has two methods:

    - `send()`: Send a string response to the client.
    - `json()`: Send a JSON response to the client.

You can respond with `json()` by providing object:

```swift
func main(req: RequestValue, res: RequestResponse) -> RequestResponse {
    res.json(data : [
        "message": "Hello Open Runtimes 👋",
        "env": req.env,
        "payload": req.payload,
        "headers": req.headers
    ])
}
```

- To handle dependencies, you need to have `Package.swift` file. Dependencies will be automatically cached and installed, so you don't need to include `.build` folder in your function.

- The default entrypoint is `Sources/index.swift`. If your entrypoint differs, make sure to provide it in the JSON body of the request: `{ "file": "app.swift" }`.

- Swift does not require `INTERNAL_RUNTIME_ENTRYPOINT` like other runtimes. In Swift, you only need to make sure a function called `main` is defined in any of your `.swift` files.

- Swift runtime currently doesn't support ARM, because there are no official ARM images.

## Authors

**Eldad Fux**

+ [https://twitter.com/eldadfux](https://twitter.com/eldadfux)
+ [https://github.com/eldadfux](https://github.com/eldadfux)

**Bradley Schofield**

+ [https://github.com/PineappleIOnic](https://github.com/PineappleIOnic)

**Jake Barnby**

+ [https://github.com/abnegate](https://github.com/abnegate)

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
