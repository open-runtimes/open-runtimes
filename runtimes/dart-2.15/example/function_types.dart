import 'dart:convert';

class FunctionRequest {
  final Map<String, dynamic> env;
  final Map<String, dynamic> headers;
  final String payload;

  FunctionRequest({
    this.env = const {},
    this.headers = const {},
    this.payload = '',
  });
}

class FunctionResponse {
  int _status = 200;
  String? _text;

  int get status => _status;
  String? get body => _text;

  FunctionResponse send(String? text, {int status = 200}) {
    _text = text;
    _status = status;
    return this;
  }

  FunctionResponse json(Map<String, dynamic> json, {int status = 200}) {
    _text = jsonEncode(json);
    _status = status;
    return this;
  }
}
