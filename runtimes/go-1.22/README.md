# Go Runtime 1.22

This is the Open Runtime that builds and runs Go code based on a `golang:1.22-alpine` base image. 

The runtime itself uses [net/http](https://pkg.go.dev/net/http) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `main.go` file:

```bash
mkdir go-function && cd go-function
tee -a main.go << END
package handler

import (
	"math/rand"
	
	"github.com/open-runtimes/types-for-go/v4"
)

func Main(Context *types.Context) types.ResponseOutput {
	return Context.Res.Json(map[string]any{
		"n": rand.Float64(),
	}, 200, nil)
}

END

```

2. Add `go.mod` file to mark function an open runtimes handler:

```bash
tee -a go.mod << END
module openruntimes/handler

require github.com/open-runtimes/types-for-go/v4 v4.0.2

END

```

3. Build the code:

```bash
docker run -e OPEN_RUNTIMES_ENTRYPOINT=main.go --rm --interactive --tty --volume $PWD:/mnt/code openruntimes/go:v4-1.22 sh helpers/build.sh
```

4. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/go:v4-1.22 sh helpers/start.sh "/usr/local/server/src/function/server"
```

5. In new terminal window, execute function:

```bash
curl -H "x-open-runtimes-secret: secret-key" -X GET http://localhost:3000/
```

Output `{"n":0.7232589496628183}` with random float will be displayed after the execution.

## Local development

1. Clone the [open-runtimes](https://github.com/open-runtimes/open-runtimes) repository:

```bash
git clone https://github.com/open-runtimes/open-runtimes.git
```

2. Enter the Go runtime folder:

```bash
cd open-runtimes/runtimes/go-1.22
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

- When writing function for this runtime, ensure it is named `Main`. An example of this is:

```go
package handler

import (
	"github.com/open-runtimes/types-for-go/v4"
)

func Main(Context *types.Context) types.ResponseOutput {
}
```

- Ensure your Go files starts with `package handler`

- To handle dependencies, you add them to `go.mod` file. Dependencies will be automatically installed.

- The default entrypoint is `main.go`. If your entrypoint differs, make sure to configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable during build, for instance, `OPEN_RUNTIMES_ENTRYPOINT=src/app.go`.

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
