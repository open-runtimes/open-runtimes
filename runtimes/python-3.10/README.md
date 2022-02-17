# Python Runtime 3.10

This is the Open Runtime that builds and runs Python code based on a `python:3.10.2-alpine` base image. 

The runtime itself uses [Flask](https://flask.palletsprojects.com/en/2.0.x) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `main.py` file:    

```bash
mkdir python-or && cd python-or
printf "import random\n\ndef main(req, res):\n    return res.json({'n': random.random() })" > main.py
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
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload": "{}"}'
```

Output `{"n":0.7232589496628183}` with random float will be displayed after the execution.

## Local development

1. Clone the [open-runtimes](https://github.com/open-runtimes/open-runtimes) repository:

```bash
git clone https://github.com/open-runtimes/open-runtimes.git
```

2. Enter the python runtime folder:

```bash
cd open-runtimes/runtimes/python-3.10
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

- When writing functions for this runtime, ensure they are exported with a name of `main`. An example of this is:

```python
def main(req, res):
    res.send('Hello Open Runtimes 👋')
}
```

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

- To handle dependencies, you need to have `requirements.txt` file. Dependencies will be automatically cached and installed, so you don't need to include `__pycache__` folder in your function.

- The default entrypoint is `main.py`. If your entrypoint differs, make sure to configure it using `INTERNAL_RUNTIME_KEY` environment variable, for instance, `INTERNAL_RUNTIME_KEY=src/app.py`.

## Authors

**Eldad Fux**

+ [https://twitter.com/eldadfux](https://twitter.com/eldadfux)
+ [https://github.com/eldadfux](https://github.com/eldadfux)

**Bradley Schofield**

+ [https://github.com/PineappleIOnic](https://github.com/PineappleIOnic)

**Matej Bačo**

+ [https://github.com/Meldiron](https://github.com/Meldiron)

**Jake Barnby**

+ [https://github.com/abnegate](https://github.com/abnegate)

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
