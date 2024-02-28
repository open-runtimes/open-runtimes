# Dart Runtime 3.1

This is the Open Runtime that builds and runs Dart code based on a `dart:3.1` base image. 

The runtime itself uses [Shelf](https://pub.dev/documentation/shelf/latest/shelf_io/shelf_io-library.html) as the Web Server to process the execution requests.

To learn more about runtimes, visit [Structure](https://github.com/open-runtimes/open-runtimes#structure) section of the main README.md.

## Usage

1. Create a folder and enter it. Add code into `lib/main.dart` file:

```bash
mkdir dart-function && cd dart-function && mkdir lib
tee -a lib/main.dart << END
import 'dart:async';
import 'dart:math';

Future<dynamic> main(final context) async {
  return context.res.json({'n': new Random().nextDouble() });
}

END

```

2. Build the code:

```bash
docker run -e OPEN_RUNTIMES_ENTRYPOINT=lib/main.dart --rm --interactive --tty --volume $PWD:/mnt/code openruntimes/dart:v3-3.1 sh helpers/build.sh
```

3. Spin-up open-runtime:

```bash
docker run -p 3000:3000 -e OPEN_RUNTIMES_SECRET=secret-key --rm --interactive --tty --volume $PWD/code.tar.gz:/mnt/code/code.tar.gz:ro openruntimes/dart:v3-3.1 sh helpers/start.sh "/usr/local/server/src/function/server"
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

2. Enter the Dart runtime folder:

```bash
cd open-runtimes/runtimes/dart-3.1
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

- When writing function for this runtime, ensure it is named `start`. An example of this is:

```dart
import 'dart:async';

Future<dynamic> main(final context) async {
  return res.send('Hello Open Runtimes 👋');
}
```

- To handle dependencies, you need to have `pubspec.yaml` file. To install those dependencies, pass `OPEN_RUNTIMES_BUILD_COMMAND="dart pub get"` during build.

- The default entrypoint is `lib/main.dart`. If your entrypoint differs, make sure to configure it using `OPEN_RUNTIMES_ENTRYPOINT` environment variable during build, for instance, `OPEN_RUNTIMES_ENTRYPOINT=lib/app.dart`.

- Dart function is a Dart library package, which means, your code has to be inside `lib` folder. You can learn more in [Dart documentation](https://dart.dev/guides/libraries/create-library-packages).

## Contributing

For security issues, please email security@appwrite.io instead of posting a public issue in GitHub.

You can refer to the [Contributing Guide](https://github.com/open-runtimes/open-runtimes/blob/main/CONTRIBUTING.md) for more info.
