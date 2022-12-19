import 'dart:convert';

class Request {
  final Map<String, dynamic> variables;
  final Map<String, dynamic> headers;
  final String payload;

  Request({
    this.variables = const {},
    this.headers = const {},
    this.payload = '',
  });
}

class Response {
  int _status = 200;
  dynamic _text;

  bool _responseSent = false;

  int get status => _status;
  dynamic get body => _text;
  bool get responseSent => _responseSent;

  Response send(String? text, {int status = 200}) {
    _responseSent = true;
    _text = text;
    _status = status;
    return this;
  }

  Response json(Map<String, dynamic> json, {int status = 200}) {
    _responseSent = true;
    _text = json;
    _status = status;
    return this;
  }
}
