# .NET Runtime 3.1

This is the Open Runtime that builds and runs .NET code based on a `mcr.microsoft.com/dotnet/sdk:3.1-alpine` base image. 

The runtime itself uses [ASP.NET Core](https://docs.microsoft.com/en-us/aspnet/core/?view=aspnetcore-3.1) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `Index.cs` file:

```bash
mkdir dotnet-function && cd dotnet-function
tee -a Index.cs << END
namespace DotNetRuntime;

public class Handler {
  public async Task<RuntimeOutput> Main(RuntimeContext Context) {
    return Context.Res.Json(new() {{ "n", new System.Random().NextDouble() }} );
  }
}

END

```

2. Build the code:

```bash
docker run -e OPEN_RUNTIMES_ENTRYPOINT=Indes.cs --rm --interactive --tty --volume $PWD:/mnt/code openruntimes/dotnet:v3-3.1 sh helpers/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/dotnet:v3-3.1 sh helpers/start.sh "dotnet /usr/local/server/src/function/DotNetRuntime.dll"
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

2. Enter the .NET runtime folder:

```bash
cd open-runtimes/runtimes/dotnet-3.1
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

- When writing function for this runtime, ensure it is named `Main` and is inside `Handler` class. An example of this is:

```cs
namespace DotNetRuntime;

public class Handler {
    public async Task<RuntimeOutput> Main(RuntimeContext Context) => 
        Context.Res.Send("Hello Open Runtimes 👋");
}
```

- Your entrypoint code must start with `namespace DotNetRuntime;`.

- To handle dependencies, you need to have `csproj` file containing the `PackageReferences` you desire. Dependencies will be automatically cached and installed, so you don't need to include the `.nuget` folder in your function.

- The default entrypoint is `Index.cs`. If your entrypoint differs, make sure to configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable, for instance, `OPEN_RUNTIMES_ENTRYPOINT=src/App.cs`.

- F# can be used in .NET runtime:

```fs
namespace DotNetRuntime

type Handler()=
  [YOUR_CODE]
```

- Visual Basic can be used in .NET runtime:

```vb
Namespace DotNetRuntime
  Public Class Handler
    [YOUR_CODE]
  End Class
End Namespace
```

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
