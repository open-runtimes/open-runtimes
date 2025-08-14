import 'dart:async';
import 'dart:convert';
import 'package:dio/dio.dart' hide Response;
import 'dart:io' show Platform;
import 'dart:typed_data';
import 'package:crypto/crypto.dart';

Future<dynamic> main(final context) async {
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
