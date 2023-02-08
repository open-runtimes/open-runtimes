# Node Runtime 18.0

This is the Open Runtime that builds and runs NodeJS code based on a `node:18.0-alpine` base image. 

The runtime itself uses [Micro](https://github.com/vercel/micro) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `index.js` file:

```bash
mkdir node-or && cd node-or
printf "module.exports = async (context) => {\n    return context.res.json({ n: Math.random() })\n}" > index.js
```

2. Build the code:

```bash
docker run --rm --interactive --tty --volume $PWD:/usr/code openruntimes/node:v3-18.0 sh -c "cp -R /usr/code/* /usr/builds && cd /usr/builds && if [ -f package.json ]; then npm install; fi && mkdir -p node_modules && cp -R /usr/local/src/node_modules/* /usr/builds/node_modules && tar --exclude code.tar.gz -zcf /usr/code/code.tar.gz ."
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key -e OPEN_RUNTIMES_ENTRYPOINT=index.js --rm --interactive --tty --volume $PWD/code.tar.gz:/tmp/code.tar.gz:ro openruntimes/node:v3-18.0 sh -c "cp /tmp/code.tar.gz /usr/workspace/code.tar.gz && cd /usr/workspace && tar -zxf /usr/workspace/code.tar.gz -C /usr/code-start && rm /usr/workspace/code.tar.gz && cp -R /usr/code-start/node_modules/* /usr/local/src/node_modules && cd /usr/local/src && npm start"
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

2. Enter the Node runtime folder:

```bash
cd open-runtimes/runtimes/node-18.0
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

- When writing function for this runtime, ensure is is exported directly through the `module.exports` object. An example of this is:

```js
module.exports = (context) => {
    return context.res.send('Hello Open Runtimes 👋');
}
```

- To handle dependencies, you need to have `package.json` file. Dependencies will be automatically cached and installed, so you don't need to include `node_modules` folder in your function.

- The default entrypoint is `index.js`. If your entrypoint differs, make sure to configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable during build, for instance, `OPEN_RUNTIMES_ENTRYPOINT=src/app.js`.


## Authors

**Eldad Fux**

+ [https://twitter.com/eldadfux](https://twitter.com/eldadfux)
+ [https://github.com/eldadfux](https://github.com/eldadfux)

**Bradley Schofield**

+ [https://github.com/PineappleIOnic](https://github.com/PineappleIOnic)

**Matej Bačo**

+ [https://github.com/Meldiron](https://github.com/Meldiron)

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
