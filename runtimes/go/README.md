# Go Runtime
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

require github.com/open-runtimes/types-for-go/v4 v4.0.0

END

```

3. Build the code:
> Examples use GO-1.22, but you can use any from `versions` directory.

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
