# Byn Runtime 0.5.9

This is the Open Runtime that builds and runs Bun code based on a `oven/bun:0.5.9` base image.

The runtime itself uses [Bun HTTP Server](https://bun.sh/docs/api/http) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `index.ts` file:

```bash
mkdir bun-or && cd bun-or
printf "export default async function (req: any, res: any) {\n  return res.json({ n: Math.random() })\n}" > index.ts
```

2. Build the code:

```bash
docker run --rm --interactive --tty --volume $PWD:/usr/code openruntimes/bun:v2-0.5.9 sh /usr/local/src/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e INTERNAL_RUNTIME_KEY=secret-key -e INTERNAL_RUNTIME_ENTRYPOINT=index.js --rm --interactive --tty --volume $PWD/code.tar.gz:/tmp/code.tar.gz:ro openruntimes/bun:v2-0.5.9 sh /usr/local/src/start.sh
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

2. Enter the Bun runtime folder:

```bash
cd open-runtimes/runtimes/bun-0.5.9
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

- You can use either `ts` or `js` extension for your function file.
- When writing function for this runtime, ensure it is exported directly through the `export default` function. An example of this is:
- Remember to return `res` object from the function.

```ts
export default async function (req: any, res: any) {
    return res.send('Hello Open Runtimes 👋');
}
```

- The `res` parameter has two methods:

    - `send()`: Send a string response to the client.
    - `json()`: Send a JSON response to the client.

You can respond with `json()` by providing object:

```ts
export default async function (req: any, res: any) {
    return res.json({
        'message'  : 'Hello Open Runtimes 👋',
        'variables': req.variables,
        'payload'  : req.payload,
        'headers'  : req.headers
    });
}
```

- To handle dependencies, you need to have `package.json` file. Dependencies will be automatically cached and installed, so you don't need to include `node_modules` folder in your function.

- The default entrypoint is `index.ts`. If your entrypoint differs, make sure to configure it using `INTERNAL_RUNTIME_ENTRYPOINT` environment variable, for instance, `INTERNAL_RUNTIME_ENTRYPOINT=src/app.js`.

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
