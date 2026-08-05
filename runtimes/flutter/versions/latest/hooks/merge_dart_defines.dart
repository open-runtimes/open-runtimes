// Merges OPEN_RUNTIMES_BUILD_VARS into an existing dart_defines.json, if any.
// Existing keys win. Usage: dart merge_dart_defines.dart <existing> <output>
import 'dart:convert';
import 'dart:io';

Map<String, dynamic> _decodeObject(String raw) {
  if (raw.trim().isEmpty) return {};
  try {
    final decoded = jsonDecode(raw);
    if (decoded is Map<String, dynamic>) return decoded;
  } catch (_) {
    // Malformed input must never fail the build; treat it as absent.
  }
  return {};
}

void main(List<String> args) {
  final existingPath = args[0];
  final outputPath = args[1];

  final appwriteVars = _decodeObject(
    Platform.environment['OPEN_RUNTIMES_BUILD_VARS'] ?? '',
  );

  final existingFile = File(existingPath);
  final existingVars = existingFile.existsSync()
      ? _decodeObject(existingFile.readAsStringSync())
      : <String, dynamic>{};

  final merged = {...appwriteVars, ...existingVars};

  if (merged.isEmpty) {
    return;
  }

  final outputFile = File(outputPath);
  outputFile.createSync(recursive: true);
  outputFile.writeAsStringSync(jsonEncode(merged));
}
