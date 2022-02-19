# PHP Runtime 8.0

This is the Open Runtime that builds and runs PHP code based on a `php:8.0.14-cli-alpine` base image. 

The runtime itself uses [Swoole](https://github.com/swoole/swoole-src) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `index.php` file:

```bash
mkdir php-or && cd php-or
printf "<?\nreturn function(\$req, \$res) { \$res->json([ 'n' => \mt_rand() / \mt_getrandmax() ]); };" > index.php
```

2. Build the code:

```bash
docker run -e ENTRYPOINT_NAME=index.php --rm --interactive --tty --volume $PWD:/usr/code openruntimes/php:8.0 sh /usr/local/src/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e INTERNAL_RUNTIME_KEY=secret-key -e INTERNAL_RUNTIME_ENTRYPOINT=index.php --rm --interactive --tty --volume $PWD/code.tar.gz:/tmp/code.tar.gz:ro openruntimes/php:8.0 sh /usr/local/src/start.sh
```

4. In new terminal window, execute function:

```bash
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload": "{}"}'
```

Output `{"n":0.7232589496628183}` with random float will be displayed after the execution.

## Local development

1. Clone the [open-runtimes](https://github.com/open-runtimes/open-runtimes) repository:

```bash
git clone https://github.com/open-runtimes/open-runtimes.git
```

2. Enter the PHP runtime folder:

```bash
cd open-runtimes/runtimes/php-8.0
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

```php
<?php

return function($req, $res) {
    $res->send('Hello Open Runtimes 👋');
};
```

- The `res` parameter has two methods:

    - `send()`: Send a string response to the client.
    - `json()`: Send a JSON response to the client.

You can respond with `json()` by providing object:

```php
return function($req, $res) {
    $res->json([
        'message' => 'Hello Open Runtimes 👋',
        'env' => $req['env'],
        'payload' => $req['payload'],
        'headers' => $req['headers']
    ]);
};
```

- To handle dependencies, you need to have `composer.json` file. Dependencies will be automatically cached and installed, so you don't need to include `vendor` folder in your function.

- If you plan to use libraries in your PHP code, make sure to include follwing like at the top of your code:

```php
require 'vendor/autoload.php';
```

- The default entrypoint is `index.php`. If your entrypoint differs, make sure to configure it using `INTERNAL_RUNTIME_ENTRYPOINT` environment variable, for instance, `INTERNAL_RUNTIME_ENTRYPOINT=src/app.php`.

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
