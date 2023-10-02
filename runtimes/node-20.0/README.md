# Node Runtime 20.0

This is the Open Runtime that builds and runs NodeJS code based on a `node:20.0-alpine3.16` base image. 

The runtime itself uses [Micro](https://github.com/vercel/micro) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `index.js` file:

```bash
mkdir node-function && cd node-function
tee -a index.js << END
module.exports = async (context) => {
    return context.res.json({ n: Math.random() });
};

END
```

2. Generate your `package.json` file:

```bash
npm init --yes
```

3. Build the code:

```bash
docker run --rm --interactive --tty -v $(pwd):/mnt/code:rw -e OPEN_RUNTIMES_ENTRYPOINT=index.js openruntimes/node:v3-20.0 sh helpers/build.sh "npm install"
```

4. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $(pwd)/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/node:v3-20.0 sh helpers/start.sh "pm2 start src/server.js --no-daemon"
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

2. Enter the Node runtime folder:

```bash
cd open-runtimes/runtimes/node-20.0
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

- When writing function for this runtime, ensure is is exported directly through the `module.exports` object. An example of this is:

```js
module.exports = (context) => {
    return context.res.send('Hello Open Runtimes 👋');
}
```

- To handle dependencies, you need to have `package.json` file. To install those dependencies, pass `OPEN_RUNTIMES_BUILD_COMMAND="npm install"` during build.

- The default entrypoint is `index.js`. If your entrypoint differs, make sure to configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable during build, for instance, `OPEN_RUNTIMES_ENTRYPOINT=src/app.js`.


## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
