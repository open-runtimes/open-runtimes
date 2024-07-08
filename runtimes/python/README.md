# Python Runtime

The runtime itself uses [Flask](https://flask.palletsprojects.com/en/2.0.x) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `main.py` file:    

```bash
mkdir python-function && cd python-function
tee -a main.py << END
import random

def main(context):
    return context.res.json({'n': random.random() })

END

```

2. Build the code:
> Examples use python-3.8, but you can use any from `versions` directory.

```bash
docker run -e OPEN_RUNTIMES_ENTRYPOINT=main.py --rm --interactive --tty --volume $PWD:/mnt/code openruntimes/python:v4-3.8 sh helpers/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/python:v4-3.8 sh helpers/start.sh "python3 src/server.py"
```

4. In new terminal window, execute function:

```bash
curl -H "x-open-runtimes-secret: secret-key" -X GET http://localhost:3000/
```

Output `{"n":0.7232589496628183}` with random float will be displayed after the execution.

## Notes

- When writing function for this runtime, ensure it is named `main`. An example of this is:

```python
def main(context):
    return context.res.send('Hello Open Runtimes 👋')
```

- To handle dependencies, you need to have `requirements.txt` file. To install those dependencies, pass `OPEN_RUNTIMES_BUILD_COMMAND="pip install -r requirements.txt"` during build.

- The default entrypoint is `main.py`. If your entrypoint differs, make sure to configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable during build, for instance, `OPEN_RUNTIMES_ENTRYPOINT=src/app.py`.

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
