# Node Runtime 17.0

This is the Open Runtime that builds and runs NodeJS code based on a `node:17-alpine` base image. 

The runtime itself uses [Micro](https://github.com/vercel/micro) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Runtimes introduction](https://github.com/open-runtimes/open-runtimes#runtimes-introduction) section of the main README.md.

# Usage

Clone the repository:

```bash
git clone https://github.com/open-runtimes/open-runtimes.git
```

Enter the node runtime folder:

```bash
cd open-runtimes/runtimes/node-17.0
```

Run the included example cloud function:

```bash
docker-compose up -d
```

You can now send `POST` request to `http://localhost:3000`. Make sure you have header `x-internal-challenge: example1234`, and JSON body `{ "path": "/usr/code", "file": "index.js" }`.

You can also make changes to the example code and apply the changes with the `docker-compose restart` command.

# Notes

When writing functions for this runtime, ensure they are exported directly through the `module.exports` object.

An example of this is:

```js
module.exports = (req, res) => {
    res.send('Hello Open Runtimes 👋');
}
```

The `res` parameter has two methods:

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
