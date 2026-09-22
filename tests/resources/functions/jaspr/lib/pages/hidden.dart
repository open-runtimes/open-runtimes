import 'package:jaspr/dom.dart';
import 'package:jaspr/jaspr.dart';

import '.config/.file.dart';

class HiddenPage extends StatelessComponent {
  const HiddenPage({super.key});

  @override
  Component build(BuildContext context) {
    return p([text(hiddenValue)]);
  }
}
