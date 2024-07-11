# Bun Runtime 1.0

The runtime itself uses Bun's built-in Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `index.ts` file:

```bash
mkdir bun-function && cd bun-function
tee -a index.ts << END
export default async function(context: any) {
    return context.res.json({ n: Math.random() });
}

END

```

2. Build the code:
> Examples use BUN-1.0, but you can use any from `versions` directory.

```bash
docker run -e OPEN_RUNTIMES_ENTRYPOINT=index.ts --rm --interactive --tty --volume $PWD:/mnt/code openruntimes/bun:v4-1.0 sh helpers/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key -e OPEN_RUNTIMES_ENTRYPOINT=index.ts --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/bun:v4-1.0 sh helpers/start.sh "bun src/server.ts"
```

4. In new terminal window, execute function:

```bash
curl -H "x-open-runtimes-secret: secret-key" -X GET http://localhost:3000/
```

Output `{"n":0.7232589496628183}` with random float will be displayed after the execution.

## Notes

- When writing function for this runtime, ensure it is exported as default one. An example of this is:

```typescript
export default async function(context: any) {
    return context.res.send('Hello Open Runtimes 👋');
}
```

- To handle dependencies, you need to have `package.json` file. To install those dependencies, pass `OPEN_RUNTIMES_BUILD_COMMAND="bun install"` during build.

- The default entrypoint is `index.ts`. If your entrypoint differs, make sure to configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable during build, for instance, `OPEN_RUNTIMES_ENTRYPOINT=src/app.ts`.

- Bun runtime currently doesn't support ARM, because there are no official ARM images.

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
