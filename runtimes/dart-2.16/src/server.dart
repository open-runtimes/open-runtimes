import 'dart:convert';
import 'dart:async';
import 'package:shelf/shelf.dart' as shelf;
import 'package:shelf/shelf_io.dart' as shelf_io;
import '{entrypoint}' as user_code;
import 'dart:io' show Platform;
import 'function_types.dart';

Future<shelf.Response> action(req) async {
  int? safeTimeout = null;
  String timeout = req.headers['x-open-runtimes-timeout'] ?? '';
  if (timeout.isNotEmpty) {
    safeTimeout = int.tryParse(timeout);
    if (safeTimeout == null || safeTimeout == 0) {
      return shelf.Response(500,
          body: 'Header "x-open-runtimes-timeout" must be an integer greater than 0.');
    }
  }

  if ((req.headers['x-open-runtimes-secret'] ?? '') == '' ||
      (req.headers['x-open-runtimes-secret'] ?? '') !=
          (Platform.environment['OPEN_RUNTIMES_SECRET'] ?? '')) {
    return shelf.Response(500,
        body:
            'Unauthorized. Provide correct "x-open-runtimes-secret" header.');
  }

  String bodyRaw = await req.readAsString();
  dynamic body = bodyRaw;
  String method = req.method;
  Map<String, dynamic> headers = {};

  for (MapEntry entry in req.headers.entries) {
    String header = entry.key.toLowerCase();
    if (!header.startsWith('x-open-runtimes-')) {
      headers[header] = entry.value;
    }
  }

  String contentType = req.headers['content-type'] ?? 'text/plain';
  if (contentType.contains('application/json')) {
    if (!bodyRaw.isEmpty) {
      body = jsonDecode(bodyRaw);
    } else {
      body = {};
    }
  }

  String hostHeader = req.headers['host'] ?? '';

  String path = req.requestedUri.path;
  String host = '';
  String scheme = req.headers['x-forwarded-proto'] ?? 'http';
  String defaultPort = scheme == 'https' ? '443' : '80';
  int port = int.parse(defaultPort);
  String queryString = req.requestedUri.query;
  Map<String, String> query = {};

  if (hostHeader.contains(':')) {
    host = hostHeader.split(':')[0];
    port = int.parse(hostHeader.split(':')[1]);
  } else {
    host = hostHeader;
    port = int.parse(defaultPort);
  }

  for (final param in queryString.split("&")) {
    final parts = param.split("=");

    final key = parts[0];
    final value = parts.sublist(1).join('=');

    if (key != null && !key.isEmpty) {
      query[key] = value;
    }
  }

  String url = scheme + '://' + host;

  if (port != int.parse(defaultPort)) {
    url += ':' + port.toString();
  }

  url += path;

  if (!queryString.isEmpty) {
    url += '?' + queryString;
  }

  RuntimeRequest contextReq = new RuntimeRequest(
      method: method,
      scheme: scheme,
      host: host,
      port: port,
      path: path,
      query: query,
      queryString: queryString,
      headers: headers,
      body: body,
      bodyRaw: bodyRaw,
      url: url);
  RuntimeResponse contextRes = new RuntimeResponse();
  RuntimeContext context = new RuntimeContext(contextReq, contextRes);

  String customstd = "";

  dynamic output = null;
  try {
    await runZoned(
      () async {
        if (safeTimeout != null) {
          dynamic result = await Future.any(<Future<dynamic>>[
            Future.delayed(Duration(seconds: safeTimeout)),
            user_code.main(context)
          ]);

          if (result != null) {
            output = result;
          } else {
            context.error('Execution timed out.');
            output = context.res.send('', 500, const {});
          }
        } else {
          output = await user_code.main(context);
        }
      },
      zoneSpecification: ZoneSpecification(
        print: (Zone self, ZoneDelegate parent, Zone zone, String line) {
          customstd += line;
        },
      ),
    );
  } catch (e, s) {
    context.error(e.toString());
    context.error(s.toString());
    output = context.res.send('', 500, const {});
  }

  if (output == null) {
    context.error(
        'Return statement missing. return context.res.empty() if no response is expected.');
    output = context.res.send('', 500, const {});
  }

  output['body'] = output['body'] ?? '';
  output['statusCode'] = output['statusCode'] ?? 200;
  output['headers'] = output['headers'] ?? {};

  Map<String, String> responseHeaders = {};

  for (MapEntry entry in output['headers'].entries) {
    String header = entry.key.toLowerCase();
    if (!header.startsWith('x-open-runtimes-')) {
      responseHeaders[header] = entry.value;
    }
  }

  String contentTypeValue = responseHeaders['content-type'] ?? 'text/plain';
  Encoding? encoding = null;

  if (contentTypeValue.contains('charset=')) {
    encoding = Encoding.getByName(contentTypeValue.split('charset=')[1]);
  } else if (!contentTypeValue.startsWith('multipart/')) {
    contentTypeValue += '; charset=utf-8';
    encoding = Encoding.getByName('utf-8');
  } 
  responseHeaders['content-type'] = contentTypeValue;

  if (!customstd.isEmpty) {
    context.log('');
    context.log(
        '----------------------------------------------------------------------------');
    context.log(
        'Unsupported logs detected. Use context.log() or context.error() for logging.');
    
    context.log(
        '----------------------------------------------------------------------------');
    context.log(customstd);
    context.log(
        '----------------------------------------------------------------------------');
  }

  responseHeaders['x-open-runtimes-logs'] =
      Uri.encodeFull(context.logs.join('\n'));
  responseHeaders['x-open-runtimes-errors'] =
      Uri.encodeFull(context.errors.join('\n'));

  return shelf.Response(output['statusCode'],
      encoding: encoding,
      body: output['body'],
      headers: responseHeaders);
}

void main() async {
  await shelf_io.serve((req) async {
    try {
      return await action(req);
    } catch (e, s) {
      List<String> logs = [];
      List<String> errors = [];

      errors.add(e.toString());
      errors.add(s.toString());

      Map<String, String> responseHeaders = {};

      responseHeaders['x-open-runtimes-logs'] =
          Uri.encodeFull(logs.join('\n'));
      responseHeaders['x-open-runtimes-errors'] =
          Uri.encodeFull(errors.join('\n'));

      return shelf.Response(500,
          encoding: Encoding.getByName('utf-8'),
          body: '',
          headers: responseHeaders);
    }
  }, '0.0.0.0', 3000);
}
