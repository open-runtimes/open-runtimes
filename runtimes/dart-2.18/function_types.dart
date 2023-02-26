import 'dart:convert';

class Request {
  String bodyString;
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

  Request(
      {String bodyString = '',
      dynamic body = '',
      Map<String, dynamic> headers = const {},
      String method = '',
      String url = '',
      String path = '',
      int port = 80,
      String scheme = '',
      String host = '',
      String queryString = '',
      Map<String, String> query = const {}})
      : bodyString = bodyString,
        body = body,
        headers = headers,
        method = method,
        url = url,
        path = path,
        port = port,
        scheme = scheme,
        host = host,
        queryString = queryString,
        query = query {}
}

class Response {
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

class Context {
  Request req;
  Response res;

  List<String> logs = [];
  List<String> errors = [];

  Context(Request req, Response res)
      : req = req,
        res = res {}

  void log(dynamic message) {
    if (message is List || message is Map) {
      this.logs.add(jsonEncode(message));
    } else {
      this.logs.add(message.toString());
    }
  }

  void error(dynamic message) {
    if (message is List || message is Map) {
      this.errors.add(jsonEncode(message));
    } else {
      this.errors.add(message.toString());
    }
  }
}
