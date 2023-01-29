import 'dart:convert';

class Request {
  String rawBody;
  dynamic body;
  Map<String, String> headers;
  String method;
  String url;

  Request({ String rawBody = '', dynamic body = '', Map<String, String> headers = const {}, String method = '', String url = '' })
    : rawBody = rawBody, body = body, headers = headers, method = method, url = url {
  }
}

class Response {
  dynamic send(String body, [int statusCode = 200, Map<String, String> headers = const {}]) {
    return {
      'body': body,
      'statusCode': statusCode,
      'headers': headers,
    };
  }

  dynamic json(Map<String, dynamic> json, [int statusCode = 200, Map<String, String> headers = const {}]) {
    var headersMerged = {
      ...headers,
      'content-type': 'application/json'
    };
    return this.send(jsonEncode(json), statusCode, headersMerged);
  }

  dynamic empty() {
    return this.send('', 204, const {});
  }

  dynamic redirect(String url, [int statusCode = 301, Map<String, String> headers = const {}]) {
    var headersMerged = {
      ...headers,
      'location': url
    };
    return this.send('', statusCode, headersMerged);
  }
}

class Context {
  Request req;
  Response res;

  List<String> _logs = [];
  List<String> _errors = [];
  	
  List<String> get logs {
    return _logs;
  }

  List<String> get errors {
    return _errors;
  }

  Context(Request req, Response res)
    : req = req, res = res {
  }

  // TODO: Support for infinite parameters
  // TODO: Support for objects (stringify)
  void log(dynamic message) {
    this._logs.add(message.toString());
  }

  void error(dynamic message) {
    this._errors.add(message.toString());
  }
}
