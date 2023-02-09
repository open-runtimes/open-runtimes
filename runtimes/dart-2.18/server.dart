import 'dart:convert';
import 'dart:async';
import 'package:shelf/shelf.dart' as shelf;
import 'package:shelf/shelf_io.dart' as shelf_io;
import '{entrypoint}' as user_code;
import 'dart:io' show Platform;
import 'function_types.dart';

// const USER_CODE_PATH = '/usr/code-start';

void main() async {
  await shelf_io.serve((req) async {
    if ((req.headers['x-open-runtimes-secret'] ?? '') == '' || (req.headers['x-open-runtimes-secret'] ?? '') != (Platform.environment['OPEN_RUNTIMES_SECRET'] ?? '')) {
      return shelf.Response(500, body: 'Unauthorized. Provide correct "x-open-runtimes-secret" header.');
    }

    String bodyString = await req.readAsString();
    dynamic body = bodyString;
    String method = req.method;
    Map<String, dynamic> headers = {};

    for (MapEntry entry in req.headers.entries) {
      String header = entry.key.toLowerCase();
      if(!header.startsWith('x-open-runtimes-')) {
        headers[header] = entry.value;
      }
    }

    String contentType = req.headers['content-type'] ?? 'plain/text';
    if(contentType.contains('application/json')) {
      if(!bodyString.isEmpty) {
        body = jsonDecode(bodyString);
      } else {
        body = {};
      }
    }

    String hostHeader = req.headers['host'] ?? '';

    String path = '/' + req.url.path;
    String host = '';
    String scheme = req.headers['x-forwarded-proto'] ?? 'http';
    String defaultPort = scheme == 'https' ? '443' : '80';
    int port = int.parse(defaultPort);
    String queryString = req.url.query;
    Map<String, String> query = {};

    if(hostHeader.contains(':')) {
      host = hostHeader.split(':')[0];
      port = int.parse(hostHeader.split(':')[1]);
    } else {
      host = hostHeader;
      port = int.parse(defaultPort);
    }

    for(final param in queryString.split("&")) {
      final parts = param.split("=");

      final key = parts[0];
      final value = parts.sublist(1).join('=');

      if(key != null && !key.isEmpty) {
        query[key] = value;
      }
    }

    String url = scheme + '://' + host;

    if(port != int.parse(defaultPort)) {
      url += ':' + port.toString();
    }

    url += path;

    if(!queryString.isEmpty) {
      url += '?' + queryString;
    }

    Request contextReq = new Request(bodyString: bodyString, body: body, headers: headers, method: method, url: url, path: path, port: port, scheme: scheme, host: host, queryString: queryString, query: query);
    Response contextRes = new Response();
    Context context = new Context(contextReq, contextRes);

    String customstd = "";

    dynamic output = null;
    try {
      await runZoned(
        () async {
          output = await user_code.handle(context);
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

    if(output == null) {
      context.error('Return statement missing. return context.res.empty() if no response is expected.');
      output = context.res.send('', 500, const {});
    }

    output['body'] = output['body'] ?? '';
    output['statusCode'] = output['statusCode'] ?? 200;
    output['headers'] = output['headers'] ?? {};

    Map<String, String> responseHeaders = {};

    for (MapEntry entry in output['headers'].entries) {
      String header = entry.key.toLowerCase();
      if(!header.startsWith('x-open-runtimes-')) {
        responseHeaders[header] = entry.value;
      }
    }

    if(!customstd.isEmpty) {
      context.log('Unsupported log noticed. Use context.log() or context.error() for logging.');
    }

    responseHeaders['x-open-runtimes-logs'] = Uri.encodeFull(context.logs.join('\n'));
    responseHeaders['x-open-runtimes-errors'] = Uri.encodeFull(context.errors.join('\n'));

    return shelf.Response(output['statusCode'], body: output['body'], headers: responseHeaders);
  }, '0.0.0.0', 3000);
}
