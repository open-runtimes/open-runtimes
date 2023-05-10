# PHP Runtime 8.1

This is the Open Runtime that builds and runs PHP code based on a `php:8.1.6-alpine` base image. 

The runtime itself uses [Swoole](https://github.com/swoole/swoole-src) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `index.php` file:

```bash
mkdir php-or && cd php-or
printf "<?\nreturn function(\$context) {\n    return \$context->res->json([ 'n' => \mt_rand() / \mt_getrandmax() ]);\n};" > index.php
```

2. Build the code:

```bash
docker run --rm --interactive --tty --volume $PWD:/usr/code openruntimes/php:v3-8.1 sh /usr/local/src/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key -e OPEN_RUNTIMES_ENTRYPOINT=index.php --rm --interactive --tty --volume $PWD/code.tar.gz:/tmp/code.tar.gz:ro openruntimes/php:v3-8.1 sh /usr/local/src/start.sh
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

2. Enter the PHP runtime folder:

```bash
cd open-runtimes/runtimes/php-8.1
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

- When writing function for this runtime, ensure it is the only returned one. An example of this is:

```php
<?php

return function($context) {
    return $context->res->send('Hello Open Runtimes 👋');
};
```

- To handle dependencies, you need to have `composer.json` file. Dependencies will be automatically cached and installed, so you don't need to include `vendor` folder in your function.

- If you plan to use libraries in your PHP code, make sure to include follwing like at the top of your code:

```php
require 'vendor/autoload.php';
```

- The default entrypoint is `index.php`. If your entrypoint differs, make sure to configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable during build, for instance, `OPEN_RUNTIMES_ENTRYPOINT=src/app.php`.

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
