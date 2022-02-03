# Node Runtime 17.0

This is the Open Runtime that both builds and runs NodeJS code.

The runtime itself uses [Micro](https://github.com/vercel/micro) as the Web Server to communicate between the Executor and the NodeJS code.

# Notes:

Functions for this runtime must be exported directly through the `module.exports` object.

An example of this is:

```js
module.exports = (req, res) => {
    res.send('Hello World!');
}
```

The res object has two methods:
`send`: Send a string response to the client.
`json`: Send a JSON response to the client.

An example of the `json` method looks like this:

```js
module.exports = (req, res) => {
    res.json({
        'normal': 'Hello World!',
        'env1': req.env['ENV1'],
        'payload': req.payload
    });
}
```

Internally the request body sent to the runtime and all runtimes looks like so:

```json
{
    "path": "/usr/code",
    "file": "index.js",

    // following will be exposed to the function
    "env": {
        // env variables
    },
    "payload": {
        // payload
    },
    "headers": {
        // headers
    }
}
```