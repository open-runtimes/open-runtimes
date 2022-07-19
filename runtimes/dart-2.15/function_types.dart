import 'dart:convert';

class Request {
  final Map<String, dynamic> env;
  final Map<String, dynamic> headers;
  final String payload;

  Request({
    this.env = const {},
    this.headers = const {},
    this.payload = '',
  });
}

class Response {
  int _status = 200;
  dynamic _text;

  int get status => _status;
  dynamic get body => _text;

  Response send(String? text, {int status = 200}) {
    _text = text;
    _status = status;
    return this;
  }

  Response json(Map<String, dynamic> json, {int status = 200}) {
    _text = json;
    _status = status;
    return this;
  }
}
