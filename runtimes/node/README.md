# Node Runtime

The runtime itself uses [Micro](https://github.com/vercel/micro) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a foler and enter it. Add code into `index.js` file:

```bash
mkdir node-or && cd node-or
printf "module.exports = async (req, res) => {\n    res.json({ n: Math.random() })\n}" > index.js
```

2. Build the code:

> Examples use Node 21.0, but you can use any from `versions` directory.

```bash
docker run --rm --interactive --tty --volume $PWD:/mnt/code:rw -e OPEN_RUNTIMES_ENTRYPOINT=index.js openruntimes/node:v4-21.0 sh helpers/build.sh "npm install"
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/node:v4-21.0 sh helpers/start.sh "pm2 start src/server.js --no-daemon"
```

4. In new terminal window, execute function:

```bash
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload": "{}"}'
```

Output `{"n":0.7232589496628183}` with random float will be displayed after the execution.

## Notes

- When writing function for this runtime, ensure is is exported directly through the `module.exports` object. An example of this is:

```js
module.exports = (req, res) => {
    res.send('Hello Open Runtimes 👋');
}
```

- The `res` parameter has two methods:

    - `send()`: Send a string response to the client.
    - `json()`: Send a JSON response to the client.

You can respond with `json()` by providing object:

```js
module.exports = (req, res) => {
    res.json({
        'message': 'Hello Open Runtimes 👋',
        'variables': req.variables,
        'payload': req.payload,
        'headers': req.headers
    });
}
```

- To handle dependencies, you need to have `package.json` file. To install those dependencies, pass `OPEN_RUNTIMES_BUILD_COMMAND="npm install"` during build.

- The default entrypoint is `index.js`. If your entrypoint differs, make sure to configure it using `INTERNAL_RUNTIME_ENTRYPOINT` environment variable, for instance, `INTERNAL_RUNTIME_ENTRYPOINT=src/app.js`.


## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
