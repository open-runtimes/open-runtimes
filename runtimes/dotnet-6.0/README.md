# .NET Runtime 6.0

This is the Open Runtime that builds and runs .NET code based on a `mcr.microsoft.com/dotnet/sdk:6.0-alpine` base image. 

The runtime itself uses [ASP.NET Core](https://docs.microsoft.com/en-us/aspnet/core/?view=aspnetcore-6.0) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `Index.cs` file:

```bash
mkdir node-or && cd node-or
printf "public async Task<RuntimeResponse> Main(RuntimeRequest req, RuntimeResponse res) => res.Json(new() {{ \"n\", new System.Random().NextDouble() }} );" > Index.cs
```

2. Build the code:

```bash
docker run --rm --interactive --tty --volume $PWD:/usr/code openruntimes/dotnet:6.0 sh /usr/local/src/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e INTERNAL_RUNTIME_KEY=secret-key -e INTERNAL_RUNTIME_ENTRYPOINT=Index.cs --rm --interactive --tty --volume $PWD/code.tar.gz:/tmp/code.tar.gz:ro openruntimes/dotnet:6.0 sh /usr/local/src/start.sh
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

2. Enter the Node runtime folder:

```bash
cd open-runtimes/runtimes/dotnet-6.0
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

```cs
public async Task<RuntimeResponse> Main(RuntimeRequest req, RuntimeResponse res) => 
    res.Json(new() {
        { "message" , "Hello Open Runtimes 👋" },
        { "variables", req.Variables },
        { "headers", req.Headers },
        { "payload", req.Payload }
    });
```

- To handle dependencies, you need to have `csproj` file containing the `PackageReferences` you desire. Dependencies will be automatically cached and installed, so you don't need to include the `.nuget` folder in your function.

- The default entrypoint is `Index.cs`. If your entrypoint differs, make sure to configure it using `INTERNAL_RUNTIME_ENTRYPOINT` environment variable, for instance, `INTERNAL_RUNTIME_ENTRYPOINT=src/App.cs`.

## Authors

**Eldad Fux**

+ [https://twitter.com/eldadfux](https://twitter.com/eldadfux)
+ [https://github.com/eldadfux](https://github.com/eldadfux)

**Jake Barnby**

+ [https://github.com/abnegate](https://github.com/abnegate)

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
