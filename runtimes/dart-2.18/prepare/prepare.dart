import 'dart:io';
import 'package:pubspec/pubspec.dart';

void main() async {
  var userPub = await PubSpec.loadFile('../user_code/pubspec.yaml');
  String packageName = userPub.name ?? 'user_code';

  print("found user package $packageName");

  var serverPub = await PubSpec.loadFile('../pubspec.yaml');
  var dependencies = serverPub.dependencies;
  dependencies[packageName] = PathReference('./user_code');

  final updatedPubspec = serverPub.copy(dependencies: dependencies);
  await updatedPubspec.save(Directory.current.parent);

  print("added user package as dependency to server.");

  File server = File("../server.dart");
  String fileName =
      Platform.environment['OPEN_RUNTIMES_ENTRYPOINT'] ?? 'lib/main.dart';
  fileName = fileName.replaceFirst('lib/', '');
  String serv = server.readAsStringSync();
  serv = serv.replaceAll('{entrypoint}', "package:$packageName/" + fileName);

  print("Updated server import of user code");

  server.writeAsStringSync(serv);
  exit(0);
}
