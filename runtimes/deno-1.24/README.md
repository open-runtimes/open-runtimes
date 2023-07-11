# Deno Runtime 1.24

This is the Open Runtime that builds and runs Deno code based on a `deno:alpine-1.24.3` base image. 

The runtime itself uses [oak](https://deno.land/x/oak@v10.6.0) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

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

```bash
docker run -e OPEN_RUNTIMES_ENTRYPOINT=mod.ts --rm --interactive --tty --volume $PWD:/mnt/code openruntimes/deno:v3-1.24 sh helpers/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key -e OPEN_RUNTIMES_ENTRYPOINT=mod.ts --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/deno:v3-1.24 sh helpers/start.sh "npm start"
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

2. Enter the Deno runtime folder:

```bash
cd open-runtimes/runtimes/deno-1.24
```

3. Run the included example cloud function:

```bash
docker-compose up -d
```

4. Execute the function:

```bash
curl -H "x-open-runtimes-secret: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"id": "4"}'
```

You can now send `POST` request to `http://localhost:3000`. Make sure you have header `x-open-runtimes-secret: secret-key`.

You can also make changes to the example code and apply the changes with the `docker-compose restart` command.

## Notes

- When writing function for this runtime, ensure it is exported as default one. An example of this is:

```typescript
export default async function(context: any) {
    return context.res.send('Hello Open Runtimes 👋');
}
```

- Dependencies are handeled automatically. Open Runtimes automatically cache and install them during build process.

- The default entrypoint is `mod.ts`. If your entrypoint differs, make sure to configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable during build, for instance, `OPEN_RUNTIMES_ENTRYPOINT=src/app.ts`.

- Deno runtime currently doesn't support ARM, because there are no official ARM images.

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
