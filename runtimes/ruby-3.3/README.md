# Ruby Runtime 3.3

This is the Open Runtime that builds and runs Ruby code based on a `ruby:3.3-alpine3.18` base image. 

The runtime itself uses [Sinatra](https://github.com/sinatra/sinatra) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `index.rb` file:

```bash
mkdir ruby-function && cd ruby-function
tee -a index.rb << END
def main(context)
    return context.res.json({:n => rand()})
end

END

```

2. Build the code:

```bash
docker run -e OPEN_RUNTIMES_ENTRYPOINT=index.rb --rm --interactive --tty --volume $PWD:/mnt/code openruntimes/ruby:v4-3. sh helpers/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/ruby:v4-3.3 sh helpers/start.sh "bundle exec puma -b tcp://0.0.0.0:3000 -e production"
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

2. Enter the Ruby runtime folder:

```bash
cd open-runtimes/runtimes/ruby-3.3
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

```ruby
def main(context)
    return context.res.send('Hello Open Runtimes 👋')
end
```

- To handle dependencies, you need to have `Gemfile` file. Dependencies will be automatically installed.

- The default entrypoint is `index.rb`. If your entrypoint differs, make sure to configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable during build, for instance, `OPEN_RUNTIMES_ENTRYPOINT=src/app.rb`.

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
