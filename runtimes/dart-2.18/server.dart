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
    if (req.headers['x-open-runtimes-secret'] != Platform.environment['OPEN_RUNTIMES_SECRET']) {
      return shelf.Response(500, body: 'Unauthorized. Provide correct "x-open-runtimes-secret" header.');
    }

    String rawBody = await req.readAsString();
    dynamic body = rawBody;
    String method = req.method;
    String url = '/' + req.url.path;
    Map<String, String> headers = {};

    if(!req.url.query.isEmpty) {
      url += '?' + req.url.query;
    }

    for (MapEntry entry in req.headers.entries) {
      String header = entry.key.toLowerCase();
      if(!header.startsWith('x-open-runtimes-')) {
        headers[header] = entry.value;
      }
    }

    String contentType = req.headers['content-type'] ?? 'plain/text';
    if(contentType.contains('application/json')) {
      body = jsonDecode(rawBody);
    }

    Request contextReq = new Request(rawBody: rawBody, body: body, headers: headers, method: method, url: url);
    Response contextRes = new Response();
    Context context = new Context(contextReq, contextRes);

    String customstd = "";

    dynamic output = null;
    try {
      await runZoned(
        () async {
          output = await user_code.start(context);
        },
        zoneSpecification: ZoneSpecification(
          print: (Zone self, ZoneDelegate parent, Zone zone, String line) {
            customstd += line;
          },
        ),
      );
    } catch (e) {
      context.error(e.toString());
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
