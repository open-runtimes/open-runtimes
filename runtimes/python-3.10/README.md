# Python Runtime 3.10

This is the Open Runtime that builds and runs Python code based on a `python:3.10-alpine` base image. 

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

```bash
docker run -e OPEN_RUNTIMES_ENTRYPOINT=main.py --rm --interactive --tty --volume $PWD:/mnt/code openruntimes/python:v3-3.10 sh helpers/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/python:v3-3.10 sh helpers/start.sh "python3 src/server.py"
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

2. Enter the Python runtime folder:

```bash
cd open-runtimes/runtimes/python-3.10
```

3. Run the included example cloud function:

```bash
docker compose up -d
```

4. Execute the function:

```bash
curl -H "x-open-runtimes-secret: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"id": "4"}'
```

You can now send `POST` request to `http://localhost:3000`. Make sure you have header `x-open-runtimes-secret: secret-key`.

You can also make changes to the example code and apply the changes with the `docker compose restart` command.

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
