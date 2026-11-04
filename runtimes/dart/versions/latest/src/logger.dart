import 'dart:math';
import 'dart:convert';
import 'dart:io';

import 'config.dart' as config;

class Logger {
  static final String TYPE_ERROR = 'error';
  static final String TYPE_LOG = 'log';
  // One byte past edge's limit, so edge sees the overflow and truncates as it does for files
  static final int INLINE_LIMIT = 5 * 1024 * 1024 + 1;

  String id = '';
  bool enabled = false;
  bool includesNativeInfo = false;
  bool inline = false;

  IOSink? streamLogs;
  IOSink? streamErrors;
  // Inline mode returns logs on the response instead of writing files
  final BytesBuilder inlineLogs = BytesBuilder();
  final BytesBuilder inlineErrors = BytesBuilder();

  Logger([String? status = null, String? id = null, bool inline = false]) {
    this.enabled = (status != null ? status : 'enabled') == 'enabled';
    this.inline = inline;

    if (this.enabled) {
      this.id = id != null
          ? id
          : (config.env == 'development' ? 'dev' : this.generateId());

      if (config.logsDirectory.isNotEmpty) {
        this.streamLogs = File(
          config.logsDirectory + '/' + this.id + '_logs.log',
        ).openWrite(mode: FileMode.append);
        this.streamErrors = File(
          config.logsDirectory + '/' + this.id + '_errors.log',
        ).openWrite(mode: FileMode.append);
      }
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

    if (stringLog.length > 8000) {
      stringLog = stringLog.substring(0, 8000);
      stringLog += "... Log truncated due to size limit (8000 characters)";
    }

    // Each sink is guarded on its own so one failing never drops the other
    try {
      (type == Logger.TYPE_ERROR ? stderr : stdout).write(stringLog + "\n");
    } catch (e) {
      // Silently fail to prevent 500 errors in runtime
      // Log write failures should not crash the runtime
    }

    if (this.inline) {
      final buffer = type == Logger.TYPE_ERROR
          ? this.inlineErrors
          : this.inlineLogs;
      final bytes = utf8.encode(stringLog + "\n");
      final room = Logger.INLINE_LIMIT - buffer.length;
      buffer.add(bytes.length > room ? bytes.sublist(0, room) : bytes);
    }

    try {
      stream?.write(stringLog + "\n");
    } catch (e) {
      // Silently fail to prevent 500 errors in runtime
    }
  }

  Future<void> end() async {
    if (!this.enabled) {
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
