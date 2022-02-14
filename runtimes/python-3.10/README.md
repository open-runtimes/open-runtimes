# Python Runtime 3.10

This is the Open Runtime that builds and runs Python code based on a `python:3.10.2-alpine3.15` base image. 

The runtime itself uses [Flask](https://flask.palletsprojects.com/en/2.0.x/) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Runtimes introduction](https://github.com/open-runtimes/open-runtimes#runtimes-introduction) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code in a new `index.py` file:

```bash
mkdir python-test && cd  python-test
echo "def main(req, res): \
    return res.json({'Hello': 'World!'}) \
" > index.py
```

2. Build the code:

```bash
docker run --rm --interactive --tty --volume $PWD:/usr/code open-runtimes/python:3.10 sh /usr/local/src/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e INTERNAL_RUNTIME_KEY=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/tmp/code.tar.gz:ro open-runtimes/python:3.10 sh /usr/local/src/start.sh
```

4. In new terminal window, execute function:

```
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload": {}}'
```

Output `{ "Hello": "World!" }` will be displayed after the execution.

## Local development

1. Clone the [open-runtimes](https://github.com/open-runtimes/open-runtimes) repository:

```bash
git clone https://github.com/open-runtimes/open-runtimes.git
```

2. Enter the Python 3.10 runtime folder:

```bash
cd open-runtimes/runtimes/python-3.10
```

3. Run the included example cloud function:

```bash
docker-compose up -d
```

4. Execute the function:

```bash
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload": {}}'
```

You can now send `POST` request to `http://localhost:3000`. Make sure you have header `x-internal-challenge: secret-key`. If your function expects any parameters, you can pass an optional JSON body like so: `{ "payload":{} }`.

You can also make changes to the example code and apply the changes with the `docker-compose restart` command.

## Notes

- The `res` parameter has two methods:

    - `send()`: Send a string response to the client.
    - `json()`: Send a JSON response to the client.

You can respond with `json()` by providing object:

```python
def main(req, res):
    res.json({
        'message': 'Hello Open Runtimes 👋',
        'env': req.env,
        'payload': req.payload,
        'headers': req.headers
    })
}
```

- To handle dependencies, you need to have a `requirements.txt` file. Dependencies will be automatically cached and installed.

- The default entrypoint is `index.py`. If your entrypoint differs, make sure to provide it in the JSON body of the request: `{ "file": "app.py" }`.

## Authors

**Eldad Fux**

+ [https://twitter.com/eldadfux](https://twitter.com/eldadfux)
+ [https://github.com/eldadfux](https://github.com/eldadfux)

**Bradley Schofield**

+ [https://github.com/PineappleIOnic](https://github.com/PineappleIOnic)

**Jake Barnby**

+ [https://github.com/abnegate](https://github.com/abnegate)

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
