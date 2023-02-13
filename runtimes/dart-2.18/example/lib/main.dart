import 'dart:async';
import 'dart:convert';
import 'package:dio/dio.dart' hide Response;

Future<dynamic> main(final context) async {

  final payload = context.req.body;

  final id = payload['id'] ?? '1';
  final todo = await Dio().get('https://jsonplaceholder.typicode.com/todos/$id');

  return context.res.json({
    'message': "Hello Open Runtimes 👋",
    'todo': todo.data,
  });
}