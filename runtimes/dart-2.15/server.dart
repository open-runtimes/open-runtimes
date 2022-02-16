import 'dart:convert';
import 'package:shelf/shelf.dart' as shelf;
import 'package:shelf/shelf_io.dart' as shelf_io;
import '{entrypoint}' as user_code;
import 'dart:io' show Platform;
import 'function_types.dart';

void main() async {
  await shelf_io.serve((req) async {
    if (req.method != 'POST') {
      return shelf.Response(500, body: 'Invalid request');
    }
    if (req.headers['x-internal-challenge'] !=
        Platform.environment['INTERNAL_RUNTIME_KEY']) {
      return shelf.Response(401,
          body: jsonEncode({'code': 401, 'message': 'Unauthorized'}));
    }
    try {
      final bodystring = await req.readAsString();
      final body = jsonDecode(bodystring);
      final request = Request(
        env: body['env'] ?? {},
        headers: body['headers'] ?? {},
        payload: body['payload'] ?? '',
      );

      final response = Response();
      await user_code.start(request, response);
      return shelf.Response.ok(response.body);
    } on FormatException catch (_) {
      return shelf.Response(500,
          body: jsonEncode({
            'code': 500,
            'message': 'Unable to properly load request body'
          }));
    } catch (e) {
      return shelf.Response(500,
          body: jsonEncode({'code': 500, 'message': e.toString()}));
    }
  }, '0.0.0.0', 3000);
}
