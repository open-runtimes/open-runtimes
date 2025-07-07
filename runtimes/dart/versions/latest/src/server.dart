import 'dart:convert';
import 'dart:async';
import 'dart:io';
import 'package:shelf/shelf.dart' as shelf;
import 'package:shelf/shelf_io.dart' as shelf_io;
import '{entrypoint}' as user_code;
import 'function_types.dart';
import 'logger.dart';

Future<shelf.Response> action(Logger logger, dynamic req) async {
  int? safeTimeout = null;
  String timeout = req.headers['x-open-runtimes-timeout'] ?? '';
  if (timeout.isNotEmpty) {
    safeTimeout = int.tryParse(timeout);
    if (safeTimeout == null || safeTimeout == 0) {
      return shelf.Response(
        500,
        body:
            'Header "x-open-runtimes-timeout" must be an integer greater than 0.',
      );
    }
  }

  if ((Platform.environment['OPEN_RUNTIMES_SECRET'] ?? '') != '' &&
      (req.headers['x-open-runtimes-secret'] ?? '') !=
          Platform.environment['OPEN_RUNTIMES_SECRET']) {
    return shelf.Response(
      500,
      body: 'Unauthorized. Provide correct "x-open-runtimes-secret" header.',
    );
  }

  int maxSize = 20 * 1024 * 1024;

  final contentLength = req.headers['content-length'];
  if (contentLength != null && int.parse(contentLength) > maxSize) {
    throw 'Request body size exceeds the size limit.';
  }

  Stream<List<int>> bodyStream = await req.read();
  List<int> bodyBinary = [];
  await for (List<int> data in bodyStream) {
    bodyBinary.addAll(data);
  }

  if (bodyBinary.length > maxSize) {
    throw 'Request body size exceeds the size limit.';
  }

  String method = req.method;
  Map<String, dynamic> headers = {};

  for (MapEntry entry in req.headers.entries) {
    String header = entry.key.toLowerCase();
    if (!header.startsWith('x-open-runtimes-')) {
      headers[header] = entry.value;
    }
  }

  String? enforcedHeadersString = Platform.environment['OPEN_RUNTIMES_HEADERS'];
  final enforcedHeaders = jsonDecode(
    (enforcedHeadersString != null && !enforcedHeadersString.isEmpty)
        ? enforcedHeadersString
        : '{}',
  );
  enforcedHeaders.forEach((key, value) {
    headers[key.toLowerCase()] = '${value}';
  });

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
    bodyBinary: bodyBinary,
    url: url,
  );
  RuntimeResponse contextRes = new RuntimeResponse();
  RuntimeContext context = new RuntimeContext(contextReq, contextRes, logger);

  dynamic output = null;
  try {
    await runZoned(
      () async {
        if (safeTimeout != null) {
          dynamic result = await Future.any(<Future<dynamic>>[
            Future.delayed(Duration(seconds: safeTimeout)),
            user_code.main(context),
          ]);

          if (result != null) {
            output = result;
          } else {
            context.error('Execution timed out.');
            output = context.res.text('', 500, const {});
          }
        } else {
          output = await user_code.main(context);
        }
      },
      zoneSpecification: ZoneSpecification(
        print: (Zone self, ZoneDelegate parent, Zone zone, String line) {
          logger.write(line, Logger.TYPE_LOG, true);
        },
      ),
    );
  } catch (e, s) {
    context.error(e.toString());
    context.error(s.toString());
    output = context.res.text('', 500, const {});
  }

  if (output == null) {
    context.error(
      'Return statement missing. return context.res.empty() if no response is expected.',
    );
    output = context.res.text('', 500, const {});
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

  String contentTypeValue = (responseHeaders['content-type'] ?? 'text/plain')
      .toLowerCase();
  Encoding? encoding = null;

  if (contentTypeValue.contains('charset=')) {
    encoding = Encoding.getByName(contentTypeValue.split('charset=')[1]);
  } else if (!contentTypeValue.startsWith('multipart/')) {
    contentTypeValue += '; charset=utf-8';
    encoding = Encoding.getByName('utf-8');
  }
  responseHeaders['content-type'] = contentTypeValue;

  responseHeaders['x-open-runtimes-log-id'] = logger.id;
  await logger.end();

  return shelf.Response(
    output['statusCode'],
    encoding: encoding,
    body: output['body'],
    headers: responseHeaders,
  );
}

void main() async {
  await shelf_io.serve(
    (req) async {
      if (req.headers['x-open-runtimes-timings'] != null) {
        String timings = await File(
          '/usr/local/telemetry/timings.txt',
        ).readAsString();
        return shelf.Response.ok(
          timings,
          headers: {'content-type': 'text/plain; charset=utf-8'},
        );
      }

      Logger logger = new Logger(
        req.headers['x-open-runtimes-logging'],
        req.headers['x-open-runtimes-log-id'],
      );

      try {
        return await action(logger, req);
      } catch (e, s) {
        logger.write(e, Logger.TYPE_ERROR);
        logger.write(s, Logger.TYPE_ERROR);

        Map<String, String> responseHeaders = {};
        responseHeaders['x-open-runtimes-log-id'] = logger.id;
        await logger.end();

        return shelf.Response(
          500,
          encoding: Encoding.getByName('utf-8'),
          body: '',
          headers: responseHeaders,
        );
      }
    },
    '0.0.0.0',
    3000,
  );

  print("HTTP server successfully started!");
}
