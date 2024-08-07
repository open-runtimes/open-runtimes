# PHP Runtime

The runtime itself uses [Swoole](https://github.com/swoole/swoole-src) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `index.php` file:

```bash
mkdir php-function && cd php-function
tee -a index.php << END
<?
return function(\$context) {
    return \$context->res->json([ 'n' => mt_rand() / mt_getrandmax() ]);
};

END

```

2. Build the code:
> Examples use PHP-8.3, but you can use any from `versions` directory.

```bash
docker run --rm --interactive --tty --volume $PWD:/mnt/code:rw -e OPEN_RUNTIMES_ENTRYPOINT=index.php openruntimes/php:v4-8.3 sh helpers/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/php:v4-8.3 sh helpers/start.sh "php src/server.php"
```

4. In new terminal window, execute function:

```bash
curl -H "x-open-runtimes-secret: secret-key" -X GET http://localhost:3000/
```

Output `{"n":0.7232589496628183}` with random float will be displayed after the execution.

## Notes

- When writing function for this runtime, ensure it is the only returned one. An example of this is:

```php
<?php

return function($context) {
    return $context->res->send('Hello Open Runtimes 👋');
};
```

- To handle dependencies, you need to have `composer.json` file. To install those dependencies, pass `OPEN_RUNTIMES_BUILD_COMMAND="composer install"` during build.

- If you plan to use libraries in your PHP code, make sure to include follwing like at the top of your code:

```php
require 'vendor/autoload.php';
```

- The default entrypoint is `index.php`. If your entrypoint differs, make sure to configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable during build, for instance, `OPEN_RUNTIMES_ENTRYPOINT=src/app.php`.

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
