# Dart Runtime 2.17

This is the Open Runtime that builds and runs Dart code based on a `dart:2.17` base image. 

The runtime itself uses [Shelf](https://pub.dev/documentation/shelf/latest/shelf_io/shelf_io-library.html) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `main.dart` file:

```bash
mkdir dart-or && cd dart-or && mkdir lib
printf "import 'dart:async';\nimport 'dart:math';\nFuture<void> start(final req, final res) async {\n    res.json({'n': new Random().nextDouble() });\n}" > lib/main.dart
```

2. Build the code:

```bash
docker run -e INTERNAL_RUNTIME_ENTRYPOINT=lib/main.dart --rm --interactive --tty --volume $PWD:/usr/code openruntimes/dart:v3-2.17 sh /usr/local/src/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e INTERNAL_RUNTIME_KEY=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/tmp/code.tar.gz:ro openruntimes/dart:v3-2.17 sh /usr/local/src/start.sh
```

4. In new terminal window, execute function:

```bash
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload":"{}"}'
```

Output `{"n":0.7232589496628183}` with random float will be displayed after the execution.

## Local development

1. Clone the [open-runtimes](https://github.com/open-runtimes/open-runtimes) repository:

```bash
git clone https://github.com/open-runtimes/open-runtimes.git
```

2. Enter the Dart runtime folder:

```bash
cd open-runtimes/runtimes/dart-2.17
```

3. Run the included example cloud function:

```bash
docker-compose up -d
```

4. Execute the function:

```bash
curl -H "X-Internal-Challenge: secret-key" -H "Content-Type: application/json" -X POST http://localhost:3000/ -d '{"payload":"{}"}'
```

You can now send `POST` request to `http://localhost:3000`. Make sure you have header `x-internal-challenge: secret-key`. If your function expects any parameters, you can pass an optional JSON body like so: `{"payload":"{}"}`.

You can also make changes to the example code and apply the changes with the `docker-compose restart` command.

## Notes

- When writing function for this runtime, ensure it is named `start`. An example of this is:

```dart
import 'dart:async';

Future<void> start(final req, final res) async {
  res.send('Hello Open Runtimes 👋');
}
```

- The `res` parameter has two methods:

    - `send()`: Send a string response to the client.
    - `json()`: Send a JSON response to the client.

You can respond with `json()` by providing object:

```dart
import 'dart:async';

Future<void> start(final req, final res) async {
  res.json({
    'message': "Hello Open Runtimes 👋",
    'variables': req.variables,
    'payload': req.payload,
    'headers': req.headers
  });
}
```

- To handle dependencies, you need to have `pubspec.yaml` file. Dependencies will be automatically cached and installed, so you don't need to include any dependencies folders in your function.

- The default entrypoint is `lib/main.dart`. If your entrypoint differs, make sure to configure it using `INTERNAL_RUNTIME_ENTRYPOINT` environment variable, for instance, `INTERNAL_RUNTIME_ENTRYPOINT=lib/app.dart`.

- Dart function is a Dart library package, which means, your code has to be inside `lib` folder. You can learn more in [Dart documentation](https://dart.dev/guides/libraries/create-library-packages).

## Authors

**Eldad Fux**

+ [https://twitter.com/eldadfux](https://twitter.com/eldadfux)
+ [https://github.com/eldadfux](https://github.com/eldadfux)

**Bradley Schofield**

+ [https://github.com/PineappleIOnic](https://github.com/PineappleIOnic)

**Matej Bačo**

+ [https://github.com/Meldiron](https://github.com/Meldiron)

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
