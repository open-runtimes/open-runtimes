import 'dart:convert';
import 'logger.dart';

class RuntimeRequest {
  List<int> bodyBinary;
  Map<String, dynamic> headers;
  String method;
  String url;
  String path;
  int port;
  String scheme;
  String host;
  String queryString;
  Map<String, String> query;

  dynamic get body {
    String contentType = (this.headers['content-type'] ?? 'text/plain').toLowerCase();

    if(contentType.startsWith("application/json")) {
      return this.bodyJson;
    }

    List<String> binaryTypes = ["application/", "audio/", "font/", "image/", "video/"];
    for(final binaryType in binaryTypes) {
      if(contentType.startsWith(binaryType)) {
        return this.bodyBinary;
      }
    }

    return this.bodyText;
  }

  String get bodyRaw {
    return this.bodyText;
  }
  
  String get bodyText {
    return utf8.decode(this.bodyBinary);
  }

  dynamic get bodyJson {
    return jsonDecode(this.bodyText);
  }

  RuntimeRequest(
      {String method = '',
      String scheme = '',
      String host = '',
      int port = 80,
      String path = '',
      Map<String, String> query = const {},
      String queryString = '',
      Map<String, dynamic> headers = const {},
      List<int> bodyBinary = const [],
      String url = '',})
      : method = method,
        scheme = scheme,
        host = host,
        port = port,
        path = path,
        query = query,
        queryString = queryString,
        headers = headers,
        bodyBinary = bodyBinary,
        url = url {}
}

class RuntimeResponse {
  dynamic binary(List<int> bytes,
      [int statusCode = 200, Map<String, dynamic> headers = const {}]) {
    return {
      'body': bytes,
      'statusCode': statusCode,
      'headers': headers,
    };
  }

  dynamic send(String body,
      [int statusCode = 200, Map<String, dynamic> headers = const {}]) {
    return this.text(body, statusCode, headers);
  }

  dynamic text(String body,
      [int statusCode = 200, Map<String, dynamic> headers = const {}]) {
    return this.binary(utf8.encode(body), statusCode, headers);
  }

  dynamic json(Map<String, dynamic> json,
      [int statusCode = 200, Map<String, dynamic> headers = const {}]) {
    var headersMerged = {...headers, 'content-type': 'application/json'};
    return this.text(jsonEncode(json), statusCode, headersMerged);
  }

  dynamic empty() {
    return this.text('', 204, const {});
  }

  dynamic redirect(String url,
      [int statusCode = 301, Map<String, dynamic> headers = const {}]) {
    var headersMerged = {...headers, 'location': url};
    return this.text('', statusCode, headersMerged);
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
