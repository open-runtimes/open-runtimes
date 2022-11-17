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
    List<String> userLogs = [];
    if (req.method != 'POST') {
      return shelf.Response(500, body: 'Invalid request');
    }
    if (req.headers['x-internal-challenge'] !=
        Platform.environment['INTERNAL_RUNTIME_KEY']) {
      return shelf.Response(500, body: 'Unauthorized');
    }
    final headers = {
      'content-type': 'application/json'
    };
    
    try {
      final bodystring = await req.readAsString();
      final body = jsonDecode(bodystring);
      final request = Request(
        variables: body['variables'] ?? {},
        headers: body['headers'] ?? {},
        payload: body['payload'] ?? '',
      );

      final response = Response();
      await runZonedGuarded(
        () async {
          await user_code.start(request, response);
        },
        (e, stackTrace) => print('$e $stackTrace'),
        zoneSpecification: ZoneSpecification(
          print: (Zone self, ZoneDelegate parent, Zone zone, String line) {
            userLogs.add(line);
          },
        ),
      );
      return shelf.Response.ok(
          jsonEncode({
            "response": response.body,
            "stdout": userLogs.join('\n'),
            "stderr": ""
          }),
          headers: headers);
    } on FormatException catch (_) {
      return shelf.Response(500, body: jsonEncode({
        'stderr': 'Unable to properly load request body',
        'stdout': userLogs.join('\n')
      }), headers: headers);
    } catch (e) {
      return shelf.Response(500, body: jsonEncode({
        'stderr': e.toString(),
        'stdout': userLogs.join('\n'),
      }), headers: headers);
    }
  }, '0.0.0.0', 3000);
}
