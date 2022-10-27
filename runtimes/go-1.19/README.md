# Go Runtime 1.19 
This is the Open Runtime that builds and runs Go code based on a `golang:1.19-alpine` base image.

The runtime itself uses [net/http](https://pkg.go.dev/net/http) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `main.go` file:
```bash
mkdir go-or && cd go-or
printf "package main\n\nimport \"math/rand\"\n\nfunc Main(req Request, res *Response) error {\n  data := make(map[string]interface{})\n  data[\"n\"]= rand.Float64()\n  res.json(data, 200)\n  return nil\n}" > main.go
```

2. Build the code:
```bash
docker run --rm --interactive --tty --volume $PWD:/usr/code openruntimes/go:v2-1.19 sh /usr/local/src/build.sh 
``` 

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e INTERNAL_RUNTIME_KEY=secret-key -e INTERNAL_RUNTIME_ENTRYPOINT=main.go --rm --interactive --tty --volume $PWD/code.tar.gz:/tmp/code.tar.gz:ro openruntimes/go:v2-1.19 sh usr/local/src/start.sh
```

4. In new terminal window, execute function:

```bash 
curl -H "X-Inrernal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d `{"payload": "{}"}`
```

Output `{"n":0.7232589496628183}` with random float will be displayed after the execution

## Local development

1. Clone [open-runtimes](https://github.com/open-runtimes/open-runtimes) repository:

```bash
git clone https://github.com/open-runtimes/open-runtimes.git
```

2. Enter the Go runtime folder:

```bash
cd open-runtimes/runtimes/go-1.19
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

```go
package main

func Main(req Request, res *Response) error {
	data := make(map[string]interface{})

	data["message"] = "Hello Open Runtimes 👋"
	data["variables"] = "req.Varaibles"
    data["headers"] = "req.Headers"
    data["payload"] = "req.Payload"

	res.json(data, 200)
	return nil
}
```

If you need to use print methods for `console log`, use format print functions like `fmt.Fprintln()` to write into `res.buffStdout` buffer:

```go
package main

func Main(req Request, res *Response) error {

	fmt.Fprint(&res.buffStdout, "Hello")
	fmt.Fprintf(&res.buffStdout, "Hello")
	fmt.Fprintln(&res.buffStdout, "Hello")
	return nil
}
```

- Make sure to name your function `Main()` and package as `main`

- To handle dependencies, you don't need to include any `go.mod` or `go.sum` files. Just import the required packages into the function. Dependencies will be automatically downloaded. 

- The default entrypoint is `main.go`. If your entrypoint differs, make sure to configure it using `INTERNAL_RUNTIME_ENTRYPOINT` environment variable, for instance, `INTERNAL_RUNTIME_ENTRYPOINT=src/app.go`.

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.
You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.