import 'dart:async';
import 'dart:io';

import 'package:dhttpd/dhttpd.dart';

Future<void> main() async {
  final server = await Dhttpd.start(
    address: '0.0.0.0',
    port: 3000,
    path: '/usr/local/server/src/function/',
  );
  final subscriptions = <StreamSubscription<ProcessSignal>>[];
  bool stopping = false;
  Future<void> shutdown(ProcessSignal signal) async {
    if (stopping) return;
    stopping = true;
    await server.destroy();
    // close() stops accepting but active responses still need the event loop.
    for (final subscription in subscriptions) {
      await subscription.cancel();
    }
  }

  subscriptions.add(ProcessSignal.sigterm.watch().listen(shutdown));
  subscriptions.add(ProcessSignal.sigint.watch().listen(shutdown));
  print('HTTP server successfully started!');
}
