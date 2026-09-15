import 'package:jaspr/jaspr.dart';

class ExceptionPage extends StatelessComponent {
  const ExceptionPage({super.key});

  @override
  Component build(BuildContext context) {
    throw Exception('Code exception occurred');
  }
}
