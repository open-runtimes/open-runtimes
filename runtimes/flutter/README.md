# Flutter Runtime

This is the Open Runtime that builds and serves Flutter web application based on a Dart image.

A Flutter version is always coupled with specific Dart version. You can learn more in [Flutter version archive](https://docs.flutter.dev/release/archive#stable-channel-linux).

The runtime uses [dhttpd](https://pub.dev/packages/dhttpd) as the web server to serve static content.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Set up Flutter project with Web target

2. Build the code:

```bash
docker run --rm --interactive --tty --volume $PWD:/mnt/code openruntimes/dart:v4-2.15 sh helpers/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/dart:v4-2.15 sh helpers/start.sh
```

4. In new terminal window, execute function:

```bash
curl -H "x-open-runtimes-secret: secret-key" -X GET http://localhost:3000/
```

Your Flutter application should be showed as a website.

## Local development

1. Clone the [open-runtimes](https://github.com/open-runtimes/open-runtimes) repository:

```bash
git clone https://github.com/open-runtimes/open-runtimes.git
```

2. Enter the Flutter runtime folder:

```bash
cd open-runtimes/runtimes/flutter
```

3. Run tests to generate new image:

```bash
sh tests.sh
```

4. Run commands mentioned in usage section, but use `open-runtimes/test-runtime` as image

You can make changes to the runtime, and to apply the changes, re-run tests from step 3 to rebuild image.

Tests also generate `runtimes/.test` directory including final structure of runtime after packaging process. This can be helpful when debugging an error.

## Notes

- Flutter can be used in combination with `static` runtime. Open Runtimes static runtime is more optimized for performance than dhttpd's static hosting

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
