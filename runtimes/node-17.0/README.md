# Node Runtime 17.0

This is the Open Runtime that builds and runs NodeJS code based on a `node:17-alpine` base image. 

The runtime itself uses [Micro](https://github.com/vercel/micro) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Runtimes introduction](https://github.com/open-runtimes/open-runtimes#runtimes-introduction) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `index.js` file:

```bash
mkdir node-random && cd node-random
echo 'module.exports = (req, res) => { res.json({ n: Math.random() }) }' > index.js
```

2. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e INTERNAL_RUNTIME_KEY=password --rm --interactive --tty --volume $PWD:/usr/deploy-code:ro open-runtimes/node:17.0 sh /usr/local/src/deploy.sh
```

3. In new terminal window, execute function:

```
curl -H "X-Internal-Challenge: password" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload": {}}'
```

Output `{"n":0.7232589496628183}` with random float will be displayed after the execution.

## Local development

1. Clone the [open-runtimes](https://github.com/open-runtimes/open-runtimes) repository:

```bash
git clone https://github.com/open-runtimes/open-runtimes.git
```

2. Enter the node runtime folder:

```bash
cd open-runtimes/runtimes/node-17.0
```

3. Run the included example cloud function:

```bash
docker-compose up -d
```

4. Execute the function:

```bash
curl -H "X-Internal-Challenge: password" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload": {}}'
```

You can now send `POST` request to `http://localhost:3000`. Make sure you have header `x-internal-challenge: password`. If your function expects any parameters, you can pass an optional JSON body like so: `{ "payload":{} }`.

You can also make changes to the example code and apply the changes with the `docker-compose restart` command.

## Notes

- When writing functions for this runtime, ensure they are exported directly through the `module.exports` object. An example of this is:

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
        'env': req.env,
        'payload': req.payload,
        'headers': req.headers
    });
}
```

- To handle dependencies, you need to have `package.json` file. Dependencies will be automatically cached and installed, so you don't need to include `node_modules` folder in your function.

- The default entrypoint is `index.js`. If your entrypoint differs, make sure to provide it in the JSON body of the request: `{"file":"src/app.js"}`.
