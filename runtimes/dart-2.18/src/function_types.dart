import 'dart:convert';
import 'logger.dart';

class RuntimeRequest {
  String bodyRaw;
  dynamic body;
  Map<String, dynamic> headers;
  String method;
  String url;
  String path;
  int port;
  String scheme;
  String host;
  String queryString;
  Map<String, String> query;

  RuntimeRequest(
      {String method = '',
      String scheme = '',
      String host = '',
      int port = 80,
      String path = '',
      Map<String, String> query = const {},
      String queryString = '',
      Map<String, dynamic> headers = const {},
      dynamic body = '',
      String bodyRaw = '',
      String url = '',})
      : method = method,
        scheme = scheme,
        host = host,
        port = port,
        path = path,
        query = query,
        queryString = queryString,
        headers = headers,
        body = body,
        bodyRaw = bodyRaw,
        url = url {}
}

class RuntimeResponse {
  dynamic send(String body,
      [int statusCode = 200, Map<String, dynamic> headers = const {}]) {
    return {
      'body': body,
      'statusCode': statusCode,
      'headers': headers,
    };
  }

  dynamic json(Map<String, dynamic> json,
      [int statusCode = 200, Map<String, dynamic> headers = const {}]) {
    var headersMerged = {...headers, 'content-type': 'application/json'};
    return this.send(jsonEncode(json), statusCode, headersMerged);
  }

  dynamic empty() {
    return this.send('', 204, const {});
  }

  dynamic redirect(String url,
      [int statusCode = 301, Map<String, dynamic> headers = const {}]) {
    var headersMerged = {...headers, 'location': url};
    return this.send('', statusCode, headersMerged);
  }
}

class RuntimeContext {
  RuntimeRequest req;
  RuntimeResponse res;
  Logger logger;

  RuntimeContext(RuntimeRequest req, RuntimeResponse res, Logger logger)
      : req = req,
        res = res,
        logger = logger {}

  void log(dynamic message) {
    this.logger.write(message, Logger.TYPE_LOG);
  }

  void error(dynamic message) {
    this.logger.write(message, Logger.TYPE_ERROR);
  }
}
