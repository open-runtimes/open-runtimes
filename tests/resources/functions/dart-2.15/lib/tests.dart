import 'dart:async';
import 'dart:convert';
import 'package:dio/dio.dart' hide Response;

/*
    'req' variable has:
        'headers' - object with request headers
        'payload' - object with request body data
        'variables' - object with function variables
    'res' variable has:
        'send(text, status: status)' - function to return text response. Status code defaults to 200
        'json(obj, status: status)' - function to return JSON response. Status code defaults to 200
    
    If an error is thrown, a response with code 500 will be returned.
*/

Future<void> start(final req, final res) async {
  final payload = jsonDecode(req.payload == '' ? '{}' : req.payload);

  final id = payload['id'] ?? '1';
  final todo = await Dio().get('https://jsonplaceholder.typicode.com/todos/$id');

  print('String1');
  print(42);
  print(4.2);
  print(true);

  print("String2");
  print("String3");
  print("String4");
  print("String5");

  res.json({
    'isTest': true,
    'message': "Hello Open Runtimes 👋",
    'header': req.headers['x-test-header'],
    'variable': req.variables['test-variable'],
    'todo': todo.data,
  });
}
