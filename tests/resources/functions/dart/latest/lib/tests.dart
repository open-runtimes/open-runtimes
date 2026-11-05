import 'dart:async';
import 'dart:convert';

import 'package:dio/dio.dart' hide Response;

import 'dart:io' show Platform;
import 'dart:typed_data';

import 'package:crypto/crypto.dart';
import 'package:shelf/shelf.dart' as shelf;

Future<dynamic> main(context) async {
  String action = context.req.headers['x-action'] ?? '';

  switch (action) {
    case 'plaintextResponse':
      {
        return context.res.text('Hello World 👋');
      }
    case 'jsonResponse':
      {
        return context.res.json({
          'json': true,
          'message': 'Developers are awesome.',
        });
      }
    case 'customCharsetResponse':
      {
        return context.res.text('ÅÆ', 200, {
          'content-type': 'text/plain; charset=iso-8859-1',
        });
      }
    case 'uppercaseCharsetResponse':
      {
        return context.res.text('ÅÆ', 200, {'content-type': 'TEXT/PLAIN'});
      }
    case 'multipartResponse':
      {
        return context.res.text(
          """--12345
Content-Disposition: form-data; name=\"partOne\"

Why just have one part?
--12345
Content-Disposition: form-data; name=\"partTwo\"

When you can have two!
--12345--""",
          200,
          {'content-type': 'multipart/form-data; boundary=12345'},
        );
      }
    case 'redirectResponse':
      {
        return context.res.redirect('https://github.com/');
      }
    case 'emptyResponse':
      {
        return context.res.empty();
      }
    case 'noResponse':
      {
        context.res.text('This should be ignored, as it is not returned.');
        break;
      }
    case 'doubleResponse':
      {
        context.res.text('This should be ignored.');
        return context.res.text('This should be returned.');
      }
    case 'enforcedHeaders':
      {
        return context.res.json({
          'x-custom': context.req.headers['x-custom'],
          'x-custom-uppercase': context.req.headers['x-custom-uppercase'],
          'x-open-runtimes-custom':
              context.req.headers['x-open-runtimes-custom'],
        });
      }
    case 'headersResponse':
      {
        return context.res.text('OK', 200, {
          'first-header': 'first-value',
          'second-header':
              context.req.headers['x-open-runtimes-custom-in-header'] ??
              'missing',
          'cookie': context.req.headers['cookie'] ?? 'missing',
          'x-open-runtimes-custom-out-header': 'third-value',
        });
      }
    case 'statusResponse':
      {
        return context.res.text('FAIL', 404);
      }
    case 'requestMethod':
      {
        return context.res.text(context.req.method);
      }
    case 'requestUrl':
      {
        return context.res.json({
          'url': context.req.url,
          'port': context.req.port,
          'path': context.req.path,
          'query': context.req.query,
          'queryString': context.req.queryString,
          'scheme': context.req.scheme,
          'host': context.req.host,
        });
      }
    case 'nativeResponseText':
      // shelf labels non-ASCII strings application/octet-stream unless told otherwise
      return shelf.Response.ok(
        'Hello Native 👋',
        headers: {'content-type': 'text/plain'},
      );
    case 'nativeResponseJson':
      return shelf.Response(
        201,
        body: jsonEncode({
          'json': true,
          'message': 'Native responses are awesome.',
        }),
        headers: {'content-type': 'application/json'},
      );
    case 'nativeResponseHeaders':
      return shelf.Response.notFound(
        'OK',
        headers: {
          'first-header': 'first-value',
          'x-open-runtimes-custom-out-header': 'third-value',
        },
      );
    case 'nativeResponseBinary':
      return shelf.Response.ok(
        Uint8List.fromList([0, 10, 255]),
        headers: {'content-type': 'application/octet-stream'},
      );
    case 'nativeResponseStream':
      return shelf.Response.ok(
        Stream<List<int>>.fromIterable([
          utf8.encode('Hello '),
          utf8.encode('Stream'),
        ]),
        headers: {'content-type': 'text/plain'},
      );
    case 'nativeResponseChunks':
      {
        Stream<List<int>> chunks() async* {
          yield utf8.encode('first\n');
          await Future.delayed(Duration(seconds: 1));
          yield utf8.encode('second\n');
          await Future.delayed(Duration(seconds: 1));
          yield utf8.encode('third\n');
        }

        return shelf.Response(
          201,
          body: chunks(),
          headers: {'x-stream': 'yes'},
        );
      }
    case 'nativeResponseStreamLong':
      {
        Stream<List<int>> chunks() async* {
          for (int i = 0; i < 10000; i++) {
            yield utf8.encode('chunk-$i\n');
          }
        }

        return shelf.Response.ok(chunks());
      }
    case 'nativeResponseStreamException':
      {
        Stream<List<int>> chunks() async* {
          yield utf8.encode('first\n');
          throw Exception('Stream failed');
        }

        return shelf.Response.ok(chunks());
      }
    case 'nativeResponseStreamErrorStart':
      {
        Stream<List<int>> chunks() async* {
          throw Exception('Stream failed early');
        }

        return shelf.Response.ok(chunks());
      }
    case 'nativeResponseStreamTimeout':
      {
        Stream<List<int>> chunks() async* {
          yield utf8.encode('first\n');
          await Future.delayed(Duration(seconds: 5));
          yield utf8.encode('second\n');
        }

        await Future.delayed(Duration(seconds: 1));
        return shelf.Response.ok(chunks());
      }
    case 'nativeResponseStreamEmpty':
      {
        Stream<List<int>> chunks() async* {}

        return shelf.Response.ok(chunks());
      }
    case 'nativeResponseCookies':
      return shelf.Response.ok(
        'OK',
        headers: {
          'set-cookie': ['first=1', 'second=2'],
        },
      );
    case 'nativeResponseEmpty':
      return shelf.Response(204);
    case 'nativeResponseRedirect':
      return shelf.Response.movedPermanently('https://github.com/');
    case 'rawRequest':
      {
        final raw = context.req.raw;
        return context.res.json({
          'method': raw.method,
          'path': raw.requestedUri.path,
          'bodyText': await raw.readAsString(),
          'controlHeader': raw.headers['x-open-runtimes-custom-header'] ?? '',
        });
      }
    case 'requestHeaders':
      {
        return context.res.json(context.req.headers);
      }
    case 'requestBodyText':
      {
        return context.res.text(context.req.bodyText);
      }
    case 'requestBodyJson':
      {
        return context.res.json(context.req.bodyJson);
      }
    case 'requestBodyBinary':
      {
        return context.res.binary(context.req.bodyBinary);
      }
    case 'requestBodyTextAuto':
      {
        return context.res.text(context.req.body);
      }
    case 'requestBodyJsonAuto':
      {
        return context.res.json(context.req.body);
      }
    case 'binaryResponse1':
      {
        List<int> bytes = [0, 10, 255];
        return context.res.binary(bytes); // List<int>
      }
    case 'binaryResponse2':
      {
        Uint8List bytes = Uint8List.fromList([0, 20, 255]);
        return context.res.binary(bytes); // Uint8List
      }
    case 'binaryResponse3':
      {
        List<int> bytes = [0, 30, 255];
        return context.res.binary(bytes); // Just a filler
      }
    case 'binaryResponse4':
      {
        List<int> bytes = [0, 40, 255];
        return context.res.binary(bytes); // Just a filler
      }
    case 'binaryResponse5':
      {
        List<int> bytes = [0, 50, 255];
        return context.res.binary(bytes); // Just a filler
      }
    case 'binaryResponseLarge':
      {
        final bytes = Uint8List.fromList(context.req.bodyBinary);
        final hash = md5.convert(bytes);
        return context.res.text(hash.toString(), 200, {
          'x-method': context.req.method,
        });
      }
    case 'streamResponse':
      {
        Stream<String> chunks() async* {
          context.log('Stream log.');
          yield 'first\n';
          await Future.delayed(Duration(seconds: 1));
          yield 'second\n';
          await Future.delayed(Duration(seconds: 1));
          yield 'third\n';
        }

        return context.res.sendStream(chunks(), 201, {'x-stream': 'yes'});
      }
    case 'streamException':
      {
        Stream<String> chunks() async* {
          yield 'first\n';
          throw Exception('Stream failed');
        }

        return context.res.sendStream(chunks());
      }
    case 'streamTimeout':
      {
        Stream<String> chunks() async* {
          yield 'first\n';
          await Future.delayed(Duration(seconds: 5));
          yield 'second\n';
        }

        await Future.delayed(Duration(seconds: 1));
        return context.res.sendStream(chunks());
      }
    case 'streamEmpty':
      {
        Stream<String> chunks() async* {}

        return context.res.sendStream(chunks(), 200);
      }
    case 'streamSingle':
      {
        Stream<String> chunks() async* {
          yield 'only\n';
        }

        return context.res.sendStream(chunks());
      }
    case 'streamLong':
      {
        Stream<String> chunks() async* {
          for (int i = 0; i < 10000; i++) {
            yield 'chunk-$i\n';
          }
        }

        return context.res.sendStream(chunks());
      }
    case 'streamBinary':
      {
        Stream<List<int>> chunks() async* {
          yield Uint8List.fromList(List<int>.generate(128, (i) => i));
          yield Uint8List.fromList(List<int>.generate(128, (i) => i + 128));
        }

        return context.res.sendStream(chunks());
      }
    case 'streamJson':
      {
        Stream<String> chunks() async* {
          for (int n = 1; n <= 3; n++) {
            if (n > 1) {
              await Future.delayed(Duration(milliseconds: 100));
            }
            yield jsonEncode({'n': n}) + '\n';
          }
        }

        return context.res.sendStream(chunks(), 200, {
          'content-type': 'application/x-ndjson',
        });
      }
    case 'streamErrorStart':
      {
        Stream<String> chunks() async* {
          throw Exception('Stream failed early');
        }

        return context.res.sendStream(chunks());
      }
    case 'envVars':
      {
        return context.res.json({
          'var': Platform.environment['CUSTOM_ENV_VAR'],
          'emptyVar': Platform.environment['NOT_DEFINED_VAR'] ?? null,
        });
      }
    case 'logs':
      {
        print('Native log');
        context.log('Debug log');
        context.error('Error log');

        context.log("Log+With+Plus+Symbol");

        context.log(42);
        context.log(4.2);
        context.log(true);

        context.log({'objectKey': 'objectValue'});
        context.log(['arrayValue']);

        context.log('A' * 9000);
        context.error('B' * 9000);

        return context.res.text('');
      }
    case 'library':
      {
        final todo = await Dio().get(
          'https://dummyjson.com/todos/' + context.req.bodyRaw,
        );
        return context.res.json({'todo': todo.data});
      }
    case 'timeout':
      {
        context.log("Timeout start.");

        await Future.delayed(Duration(seconds: 3));

        context.log("Timeout end.");
        return context.res.text("Successful response.");
      }
    case 'deprecatedMethods':
      {
        return context.res.send(context.req.bodyRaw);
      }
    case 'deprecatedMethodsUntypedBody':
      {
        return context.res.send("50"); // Send only supported String
      }
    case 'errorTest':
      {
        context.log('Before error...');
        throw new Exception('Error!');
      }
    default:
      {
        throw new Exception('Unknown action');
      }
  }
}
