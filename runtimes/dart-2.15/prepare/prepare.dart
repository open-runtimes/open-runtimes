import 'dart:io';
import 'package:pubspec/pubspec.dart';

void main() async {
  var userPub = await PubSpec.loadFile('../user_code/pubspec.yaml');
  String packageName = userPub.name ?? 'user_code';

  var serverPub = await PubSpec.loadFile('../pubspec.yaml');
  var dependencies = serverPub.dependencies;
  dependencies[packageName] = PathReference('./user_code');

  final updatedPubspec = serverPub.copy(dependencies: dependencies);
  await updatedPubspec.save(Directory.current.parent);

  File server = File("../server.dart");
  String serv = server.readAsStringSync();
  serv = serv.replaceAll('{entrypoint}', "package:$packageName/main.dart");
  server.writeAsStringSync(serv);
  exit(0);
}
