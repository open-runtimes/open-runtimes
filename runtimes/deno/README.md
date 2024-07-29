# Deno Runtime

The runtime itself uses [oak](https://deno.land/x/oak@v10.6.0) as the Web Server
to process the execution requests.

To learn more about runtimes, visit
[Structure](https://github.com/open-runtimes/open-runtimes#structure) section of
the main README.md.

## Usage

1. Create a folder and enter it. Add code into `mod.ts` file:

```bash
mkdir deno-function && cd deno-function
tee -a mod.ts << END
export default async function(context: any) {
    return context.res.json({ n: Math.random() });
}

END
```

2. Build the code:

> Examples use Deno-1.21, but you can use any from `versions` directory.

```bash
docker run -e OPEN_RUNTIMES_ENTRYPOINT=mod.ts --rm --interactive --tty --volume $PWD:/mnt/code openruntimes/deno:v4-1.21 sh helpers/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key -e OPEN_RUNTIMES_ENTRYPOINT=mod.ts --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/deno:v4-1.21 sh helpers/start.sh "denon run --allow-net --allow-write --allow-read --allow-env src/server.ts"
```

4. In new terminal window, execute function:

```bash
curl -H "x-open-runtimes-secret: secret-key" -X GET http://localhost:3000/
```

Output `{"n":0.7232589496628183}` with random float will be displayed after the
execution.

## Notes

- When writing function for this runtime, ensure it is exported as default one.
  An example of this is:

```typescript
export default async function (context: any) {
  return context.res.send("Hello Open Runtimes 👋");
}
```

- Dependencies are handeled automatically. To install those dependencies during
  build, pass `OPEN_RUNTIMES_BUILD_COMMAND="deno cache YOUR_ENTRYPOINT"` during
  build.

- The default entrypoint is `mod.ts`. If your entrypoint differs, make sure to
  configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable during
  build, for instance, `OPEN_RUNTIMES_ENTRYPOINT=src/app.ts`.

- Deno runtime currently doesn't support ARM, because there are no official ARM
  images.

## Contributing

For security issues, please email security@appwrite.io instead of posting a
public issue in GitHub.

You can refer to the
[Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md)
for more info.
