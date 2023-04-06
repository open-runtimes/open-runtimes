# Pascal Runtime 3.2

This is the Open Runtime that builds and runs Dart code based on a `freepascal/fpc:3.2.2-focal-full` base image (Ubuntu). 

The runtime itself uses [fphttpserver](https://wiki.freepascal.org/fphttpserver) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `Index.pas` file:

```bash
mkdir pascal-or && cd pascal-or
printf "procedure Main(var RuntimeRequest: TRuntimeRequest; var RuntimeResponse: TRuntimeResponse);\nbegin\n  RuntimeResponse.Send('Hello World!')\nend;" > Index.pas
```

2. Build the code:

```bash
docker run --rm --interactive --tty --volume $PWD:/usr/code openruntimes/pascal:3.2 sh /usr/local/src/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e INTERNAL_RUNTIME_KEY=secret-key -e INTERNAL_RUNTIME_ENTRYPOINT=Index.pas --rm --interactive --tty --volume $PWD/code.tar.gz:/tmp/code.tar.gz:ro openruntimes/pascal:3.2 sh /usr/local/src/start.sh
```

4. In new terminal window, execute function:

```bash
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload": "{}"}'
```

Output `Hello world!` will be displayed after the execution.

## Local development

1. Clone the [open-runtimes](https://github.com/open-runtimes/open-runtimes) repository:

```bash
git clone https://github.com/open-runtimes/open-runtimes.git
```

2. Enter the Pascal runtime folder:

```bash
cd open-runtimes/runtimes/pascal-3.2
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

- When writing a function for this runtime, ensure it is named `Main`. An example of this is:

```pascal
procedure Main(var RuntimeRequest: TRuntimeRequest; var RuntimeResponse: TRuntimeResponse);
begin
  RuntimeResponse.send('Hello Open Runtimes 👋');
end;
```

- The `res` parameter has two methods:

    - `Send()`: Send a string response to the client.
    - `Json()`: Send a JSON response to the client.

You can respond with `Json()` by providing object:

```pascal
procedure Main(var RuntimeRequest: TRuntimeRequest; var RuntimeResponse: TRuntimeResponse);
begin
  RuntimeResponse.Json(TJSONObject.Create([
    'message', 'Hello Open Runtimes 👋',
    'variables', res.Variables,
    'payload', res.Payload,
    'headers', res.Headers,
  ]));
end;
```

- To handle imports, you may add a `uses` definition to your code. Used units will automatically be deduped, so no need to worry about that.

- **Installing external dependencies is not supported at this time!**

- The default entrypoint is `Index.pas`. If your entrypoint differs, make sure to configure it using `INTERNAL_RUNTIME_ENTRYPOINT` environment variable, for instance, `INTERNAL_RUNTIME_ENTRYPOINT=src/App.pas`.

## Authors

**Eldad Fux**

+ [https://twitter.com/eldadfux](https://twitter.com/eldadfux)
+ [https://github.com/eldadfux](https://github.com/eldadfux)

**Bradley Schofield**

+ [https://github.com/PineappleIOnic](https://github.com/PineappleIOnic)

**Matej Bačo**

+ [https://github.com/Meldiron](https://github.com/Meldiron)

**Jake Barnby**

+ [https://github.com/abnegate](https://github.com/abnegate)

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
