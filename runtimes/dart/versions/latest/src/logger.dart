import 'dart:math';
import 'dart:convert';
import 'dart:io';

class Logger {
  static final String TYPE_ERROR = 'error';
  static final String TYPE_LOG = 'log';

  String id = '';
  bool enabled = false;
  bool includesNativeInfo = false;

  IOSink? streamLogs;
  IOSink? streamErrors;

  Logger([String? status = null, String? id = null]) {
    this.enabled = (status != null ? status : 'enabled') == 'enabled';

    if (this.enabled) {
      this.id = id != null
          ? id
          : (Platform.environment['OPEN_RUNTIMES_ENV'] == 'development'
                ? 'dev'
                : this.generateId());

      this.streamLogs = File(
        '/mnt/logs/' + this.id + '_logs.log',
      ).openWrite(mode: FileMode.append);
      this.streamErrors = File(
        '/mnt/logs/' + this.id + '_errors.log',
      ).openWrite(mode: FileMode.append);
    }
  }

  void write(dynamic message, [String? type = null, bool isNative = false]) {
    type ??= Logger.TYPE_LOG;

    if (!this.enabled) {
      return;
    }

    if (isNative && !this.includesNativeInfo) {
      this.includesNativeInfo = true;
      this.write(
        'Native logs detected. Use context.log() or context.error() for better experience.',
      );
    }

    final stream = type == Logger.TYPE_ERROR
        ? this.streamErrors
        : this.streamLogs;

    if (stream == null) {
      return;
    }

    String stringLog = "";

    if (message is List || message is Map) {
      try {
        stringLog = jsonEncode(message);
      } catch (e, s) {
        stringLog = message.toString();
      }
    } else {
      stringLog = message.toString();
    }

    stream.write(stringLog + "\n");
  }

  Future<void> end() async {
    if (!this.enabled || this.streamLogs == null || this.streamErrors == null) {
      return;
    }

    this.enabled = false;

    var futureFlushes = <Future>[];
    futureFlushes.add(this.streamLogs?.flush() ?? Future.value());
    futureFlushes.add(this.streamErrors?.flush() ?? Future.value());
    await Future.wait(futureFlushes);

    var futureCloses = <Future>[];
    futureCloses.add(this.streamLogs?.close() ?? Future.value());
    futureCloses.add(this.streamErrors?.close() ?? Future.value());
    await Future.wait(futureCloses);
  }

  // Recreated from https://www.php.net/manual/en/function.uniqid.php
  String generateId({int padding = 7}) {
    final now = DateTime.now();
    final sec = (now.millisecondsSinceEpoch / 1000).floor();
    final usec = now.microsecondsSinceEpoch - (sec * 1000000);
    String id = sec.toRadixString(16) + usec.toRadixString(16).padLeft(5, '0');

    if (padding > 0) {
      StringBuffer sb = StringBuffer();
      for (var i = 0; i < padding; i++) {
        sb.write(Random().nextInt(16).toRadixString(16));
      }

      id += sb.toString();
    }

    return id;
  }
}
