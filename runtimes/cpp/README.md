
# C++ Runtime

The runtime itself uses [Drogon](https://github.com/drogonframework/drogon) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `index.cc` file:


```bash
mkdir cpp-function && cd cpp-function
tee -a index.cc << END
#include "RuntimeResponse.h"
#include "RuntimeRequest.h"
#include "RuntimeOutput.h"
#include "RuntimeContext.h"

namespace runtime {
    class Handler {
    public:
        static RuntimeOutput main(RuntimeContext &context)
        {
            Json::Value result;
            result["n"] = rand() / (RAND_MAX + 1.);
            
            return context.res.json(result);
        }
    };
}

END

```

2. Build the code:
> Examples use CPP-1.0, but you can use any from `versions` directory.

```bash
docker run -e OPEN_RUNTIMES_ENTRYPOINT=index.cc --rm --interactive --tty --volume $PWD:/mnt/code openruntimes/cpp:v4-17 sh helpers/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/cpp:v4-17 sh helpers/start.sh "/usr/local/server/src/function/cpp_runtime"
```

4. In new terminal window, execute function:

```bash
curl -H "x-open-runtimes-secret: secret-key" -X GET http://localhost:3000/
```

Output `{"n":0.7232589496628183}` with random float will be displayed after the execution.

## Notes

- When writing function for this runtime, ensure function named `main` exists in `Handler` class under `runtime` namespace. An example of this is:

```cpp
#include "RuntimeOutput.h"
#include "RuntimeContext.h"

namespace runtime {
    class Handler {
    public:
        static RuntimeOutput main(RuntimeContext &context)
        {
            auto res = context.res;
            return res.send("Hello Open Runtimes 👋");
        }
    };
}
```

- To handle dependencies, you need to include a `CMakeLists.txt` file. Dependencies will be automatically installed.

- The default entrypoint is `index.cc`. If your entrypoint differs, make sure to configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable during build, for instance, `OPEN_RUNTIMES_ENTRYPOINT=src/app.cc`.

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
