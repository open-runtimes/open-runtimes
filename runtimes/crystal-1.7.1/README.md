# Crystal Runtime 1.7.1

This is the Open Runtime that builds and runs Crystal code based on a `crystal:1.7.1-alpine` base image.

The runtime itself uses [Grip](https://github.com/grip-framework/grip) as the web server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.

## Usage

1. Create a new Crystal project and enter the project directory.

2. Rename the generated file in `/src` to `main.cr`.

3. Add the following code to `main.cr`:

```crystal
require "http/client"
require "json"

module App
  def self.exec(req, res)
    payload = JSON.parse(req.payload)
    id = payload.as_h.fetch("id", 1)

    response = HTTP::Client.get("https://jsonplaceholder.typicode.com/todos/" + id.to_s)
    todo = JSON.parse(response.body)

    return res.json({
      "message" => "Hello Open Runtimes 👋",
      "todo"    => todo,
    })
  end
end
```

4. Build the code:

```bash
docker run -e INTERNAL_RUNTIME_ENTRYPOINT=src/main.cr --rm --interactive --tty --volume $PWD:/usr/code openruntimes/crystal:1.7.1 sh /usr/local/src/build.sh
```

5. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e INTERNAL_RUNTIME_KEY=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/tmp/code.tar.gz:ro openruntimes/crystal:1.7.1 sh /usr/local/src/start.sh
```

6. In new terminal window, execute function:

```bash
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload":"{}"}'
```

Output `{"message":"Hello Open Runtimes 👋","todo":{"userId":1,"id":1,"title":"delectus aut autem","completed":false}}` will be displayed after the execution.

## Local development

1. Clone the [open-runtimes](

2. Enter the Crystal runtime folder:

```bash
cd open-runtimes/runtimes/crystal-1.7.1
```

3. Run the included example cloud function:

```bash
docker compose up -d
```

4. In new terminal window, execute function:

```bash
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload":"{}"}'
```

You can now send `POST` request to `http://localhost:3000`. Make sure you have header `x-internal-challenge: secret-key`. If your function expects any parameters, you can pass an optional JSON body like so: `{"payload":"{}"}`.

You can also make changes to the example code and apply the changes with the `docker-compose restart` command.

## Notes

- When writing a function for this runtime, ensure:
      - The function is exported as `App.exec` and accepts two arguments: `req` and `res`.
      - The function returns text using `res.send` or JSON using `res.json`.

- The `req` object has the following properties:
      - `req.payload` - Raw string payload of the request.
      - `req.variables` - Variables of the request.
      - `req.headers` - Headers of the request.

- The `res` object has the following methods:
      - `res.send` - Function to send text response.
      - `res.json` - Function to send JSON response.

- Dependencies can be handled as normal, using your `shard.yml` file.

## Authors

**Chris Watson**

+ [https://watzon.tech](https://watzon.tech)
+ [https://github.com/watzon](https://github.com/watzon)

## Contibuting

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.