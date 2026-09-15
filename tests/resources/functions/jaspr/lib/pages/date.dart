import 'package:jaspr/dom.dart';
import 'package:jaspr/jaspr.dart';

class DatePage extends StatelessComponent {
  const DatePage({super.key});

  @override
  Component build(BuildContext context) {
    final date = DateTime.now().toUtc().toIso8601String();
    return p([text('[DATE_START]$date[DATE_END]')]);
  }
}
