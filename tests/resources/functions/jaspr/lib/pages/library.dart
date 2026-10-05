import 'package:jaspr/dom.dart';
import 'package:jaspr/jaspr.dart';
import 'package:uuid/uuid.dart';

class LibraryPage extends StatelessComponent {
  const LibraryPage({super.key});

  @override
  Component build(BuildContext context) {
    final id = const Uuid().v4();
    return p([text('[UUID_START]My UUID is: $id[UUID_END]')]);
  }
}
