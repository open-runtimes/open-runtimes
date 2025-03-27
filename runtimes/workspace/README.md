# Workspace Runtime

The runtime extends the Node.js runtime to provide an interactive development environment. It uses [Micro](https://github.com/vercel/micro) as the Web Server and integrates [xterm.js](https://xtermjs.org/) for terminal emulation along with Synapse, a custom Operating system gateway for remote serverless environments. Synapse provides a WebSocket-based interface to interact with terminal sessions, manage files, and monitor system resources remotely.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `index.js` file:

```bash
mkdir workspace-demo && cd workspace-demo
printf "module.exports = async (req, res) => {\n    res.json({ n: Math.random() })\n}" > index.js
```

2. Build the code:

> Examples use Node 21.0, but you can use any from `versions` directory.

```bash
docker run --rm --interactive --tty --volume $PWD:/mnt/code:rw -e OPEN_RUNTIMES_ENTRYPOINT=index.js openruntimes/workspace:v4-21.0 sh helpers/build.sh "npm install"
```

3. Spin-up workspace runtime:

```bash
docker run -p 3000:3000 -p 8080:8080 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/workspace:v4-21.0 sh helpers/start.sh
```

4. Connect to the workspace:

The workspace will be available at `http://localhost:8080`, providing:
- Interactive terminal access through xterm.js
- File system operations via Synapse
- System resource monitoring

You can also execute functions as in the standard runtime:

```bash
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload": "{}"}'
```

## Notes

- When writing function for this runtime, ensure it is exported directly through the `module.exports` object. An example of this is:

```js
module.exports = (req, res) => {
  res.send("Hello Open Runtimes 👋");
};
```

- The `res` parameter has two methods:

  - `send()`: Send a string response to the client.
  - `json()`: Send a JSON response to the client.

You can respond with `json()` by providing object:

```js
module.exports = (req, res) => {
  res.json({
    message: "Hello Open Runtimes 👋",
    variables: req.variables,
    payload: req.payload,
    headers: req.headers,
  });
};
```

- To handle dependencies, you need to have `package.json` file. To install those dependencies, pass `OPEN_RUNTIMES_BUILD_COMMAND="npm install"` during build.

- The default entrypoint is `index.js`. If your entrypoint differs, make sure to configure it using `INTERNAL_RUNTIME_ENTRYPOINT` environment variable, for instance, `INTERNAL_RUNTIME_ENTRYPOINT=src/app.js`.

- The workspace runtime exposes WebSocket endpoints through Synapse:
  - `ws://localhost:8080/terminal` - For terminal sessions
  - `ws://localhost:8080/fs` - For file operations
  - `ws://localhost:8080/monitor` - For system monitoring

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
