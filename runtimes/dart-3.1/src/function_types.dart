import 'dart:convert';

class RuntimeRequest {
  dynamic bodyRaw;
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
      dynamic bodyRaw = '',
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

  List<String> logs = [];
  List<String> errors = [];

  RuntimeContext(RuntimeRequest req, RuntimeResponse res)
      : req = req,
        res = res {}

  void log(dynamic message) {
    if (message is List || message is Map) {
      try {
        this.logs.add(jsonEncode(message));
      } catch (e, s) {
        this.logs.add(message.toString());
      }
    } else {
      this.logs.add(message.toString());
    }
  }

  void error(dynamic message) {
    if (message is List || message is Map) {
      try {
        this.errors.add(jsonEncode(message));
      } catch (e, s) {
        this.errors.add(message.toString());
      }
    } else {
      this.errors.add(message.toString());
    }
  }
}
