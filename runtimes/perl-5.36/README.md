# Perl Runtime 5.36

This is the Open Runtime that builds and runs Perl code based on the `perl:5.36-slim` base image.

## Usage

1. Create a folder and enter it. Add code into `index.pl` file:

```bash
mkdir perl-or && cd perl-or;
touch cpanfile;
cat << EOL > index.pl
#!/usr/bin/env perl

sub main {
   my (\$req, \$res) = @_;

   \$res->json({ 
      n => rand(),
   });
}
1;
EOL
```

2. Build the code:

```bash
docker run --rm --interactive --tty \
  -e INTERNAL_RUNTIME_KEY=secret-key \
  -e INTERNAL_RUNTIME_ENTRYPOINT=index.pl \
  --volume $PWD:/usr/code  \
  openruntimes/perl:5.36 sh /usr/local/src/build.sh
```

3. Spin-up open-runtime:

```bash
docker run --rm --interactive --tty \
  -p 3000:3000 \
  -e INTERNAL_RUNTIME_KEY=secret-key \
  -e INTERNAL_RUNTIME_ENTRYPOINT=index.pl \
  --volume $PWD/code.tar.gz:/tmp/code.tar.gz:ro \
  openruntimes/perl:5.36 sh /usr/local/src/start.sh
```

4. In new terminal window, execute function:

```bash
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload": "{}"}'
```

You should expect a random float `n` to be returned after the execution.

```json
{"response":{"n":0.698343277859163},"stdout":""}
```

## Local development

1. Clone the [open-runtimes](https://github.com/open-runtimes/open-runtimes) repository:

```bash
git clone https://github.com/open-runtimes/open-runtimes.git
```

2. Enter the Perl runtime folder:

```bash
cd open-runtimes/runtimes/perl-5.36
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

- When writing function for this runtime, ensure it is the only returned one. An example of this is:

```perl
#!/usr/bin/env perl

sub main {
   my ($req, $res) = @_;

   $res->send("Hello Open Runtimes!");
}
1;
```

- The `res` parameter has two methods:

  - `send()`: Send a string response to the client.
  - `json()`: Send a JSON response to the client.

You can respond with `json()` by passing a hash:

```perl
#!/usr/bin/env perl

sub main {
   my ($req, $res) = @_;

   $res->json({ 
      message => "Hello Open Runtimes!",
   });
}
1;
```

- To handle dependencies, you need to have the `cpanfile` file. Dependencies will be automatically cached and installed, so you don't need to include the `local` folder in your function. An example of an entry in `cpanfile` is shown below:

```txt
requires 'JSON::XS';
```

- If you plan to use libraries in your Perl code, make sure to include follwing like at the top of your code:

```perl
use JSON::XS;
```

- The default entrypoint is `index.pl`. If your entrypoint differs, make sure to configure it using `INTERNAL_RUNTIME_ENTRYPOINT` environment variable, for instance, `INTERNAL_RUNTIME_ENTRYPOINT=src/app.pl`.

## Authors

**Sudipto Ghosh**

+ [https://twitter.com/ScientificGhosh](https://twitter.com/ScientificGhosh)
+ [https://github.com/sudiptog81](https://github.com/sudiptog81)

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
