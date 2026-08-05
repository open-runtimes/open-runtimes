// Merges Appwrite-provided vars (OPEN_RUNTIMES_BUILD_VARS, a JSON object of
// strings) with a dart_defines.json the user may have committed to their own
// repo. Keys already present in the user's file always win — Appwrite only
// fills in what's missing. Writes nothing if the merged result is empty, so
// callers can tell "no vars" from "vars written" by checking file existence.
//
// Usage: dart merge_dart_defines.dart <existing_path> <output_path>
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

  // Map spread: later entries win on key conflict, so existingVars overrides
  // appwriteVars.
  final merged = {...appwriteVars, ...existingVars};

  if (merged.isEmpty) {
    return;
  }

  final outputFile = File(outputPath);
  outputFile.createSync(recursive: true);
  outputFile.writeAsStringSync(jsonEncode(merged));
}
