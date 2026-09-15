library;

import 'dart:io';

import 'package:jaspr/server.dart';
import 'package:shelf/shelf.dart';
import 'package:shelf/shelf_io.dart' as shelf_io;
import 'package:shelf_router/shelf_router.dart';

import 'app.dart';
import 'pages/date.dart';
import 'pages/exception.dart';
import 'pages/hidden.dart';
import 'pages/library.dart';
import 'main.server.options.dart';

void main() async {
  Jaspr.initializeApp(options: defaultServerOptions);

  final cacheHeader =
      Platform.environment['OPEN_RUNTIMES_CACHE_HEADER'] ?? 'CDN-Cache-Control';

  final router = Router();
  router.mount(
    '/date',
    serveApp(
      (request, render) =>
          render(Document(title: 'jaspr_app', body: const DatePage())),
    ),
  );
  router.mount(
    '/exception',
    serveApp(
      (request, render) =>
          render(Document(title: 'jaspr_app', body: const ExceptionPage())),
    ),
  );
  router.mount(
    '/library',
    serveApp(
      (request, render) =>
          render(Document(title: 'jaspr_app', body: const LibraryPage())),
    ),
  );
  router.mount(
    '/hidden',
    serveApp(
      (request, render) =>
          render(Document(title: 'jaspr_app', body: const HiddenPage())),
    ),
  );
  router.mount(
    '/',
    serveApp(
      (request, render) => render(Document(title: 'jaspr_app', body: App())),
    ),
  );

  final cacheMiddleware = createMiddleware(
    responseHandler: (response) {
      final contentType = response.headers['content-type'] ?? '';
      if (contentType.startsWith('text/html')) {
        return response;
      }
      return response.change(headers: {cacheHeader: 'public, max-age=36000'});
    },
  );

  final oprMiddleware = createMiddleware(
    requestHandler: (request) async {
      if (request.url.path == '__opr/health') {
        return Response.ok('OK', headers: {'content-type': 'text/plain'});
      }
      if (request.url.path == '__opr/timings') {
        final timings = await File('/mnt/telemetry/timings.txt').readAsString();
        return Response.ok(
          timings,
          headers: {'content-type': 'text/plain; charset=utf-8'},
        );
      }
      return null;
    },
  );

  final secret = Platform.environment['OPEN_RUNTIMES_SECRET'] ?? '';

  final authMiddleware = createMiddleware(
    requestHandler: (request) {
      if (secret.isEmpty) {
        return null;
      }

      final headerSecret = request.headers['x-open-runtimes-secret'] ?? '';
      if (headerSecret != secret) {
        return Response(
          500,
          body:
              'Unauthorized. Provide correct "x-open-runtimes-secret" header.',
          headers: {'content-type': 'text/plain'},
        );
      }

      return null;
    },
  );

  final handler = const Pipeline()
      .addMiddleware(oprMiddleware)
      .addMiddleware(authMiddleware)
      .addMiddleware(cacheMiddleware)
      .addHandler(router);

  final port = int.parse(Platform.environment['PORT'] ?? '8080');
  final server = await shelf_io.serve(
    handler,
    InternetAddress.anyIPv4,
    port,
    shared: true,
  );

  print('Serving at http://${server.address.host}:${server.port}');
}
