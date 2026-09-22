import 'dart:convert';
import 'dart:io';

final String secret = Platform.environment['OPEN_RUNTIMES_SECRET'] ?? '';
final String env = Platform.environment['OPEN_RUNTIMES_ENV'] ?? '';
final String logsDirectory =
    Platform.environment['OPEN_RUNTIMES_LOGS_DIRECTORY'] ?? '/mnt/logs';
final Map<String, dynamic> headers = () {
  try {
    return jsonDecode(Platform.environment['OPEN_RUNTIMES_HEADERS'] ?? '{}')
        as Map<String, dynamic>;
  } catch (e) {
    return <String, dynamic>{};
  }
}();
