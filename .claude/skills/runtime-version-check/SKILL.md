---
name: runtime-version-check
description: Research and verify the latest versions of programming language runtimes (Node, Bun, Deno, Python, Go, etc.) using a two-phase verification process that prioritizes accuracy over speed.
---

# Runtime Version Check

A systematic process for researching and verifying the latest versions of programming language runtimes. This skill prioritizes **accuracy over speed**.

## When to Use This Skill

Use this skill when the user:

- Asks about the latest version of a programming runtime
- Needs to verify current runtime versions for upgrades
- Wants a comprehensive overview of runtime versions
- Asks to check if their runtime versions are up to date

## Supported Runtimes & Authoritative Sources

| Runtime | Primary Source (MUST verify here) | Secondary Source |
|---------|----------------------------------|------------------|
| **Bun** | https://github.com/oven-sh/bun/releases | https://bun.sh |
| **C++** | https://isocpp.org/std/status | https://en.cppreference.com |
| **Dart** | https://dart.dev/get-dart/archive | https://github.com/dart-lang/sdk/tags |
| **Deno** | https://github.com/denoland/deno/releases | https://deno.com |
| **Dotnet (.NET)** | https://dotnet.microsoft.com/en-us/download/dotnet | https://github.com/dotnet/core/releases |
| **Flutter** | https://docs.flutter.dev/release/release-notes | https://github.com/flutter/flutter/tags |
| **Go** | https://go.dev/dl/ | https://github.com/golang/go/tags |
| **Java** | https://www.oracle.com/java/technologies/downloads/ | https://www.java.com/releases/ |
| **Kotlin** | https://github.com/JetBrains/kotlin/releases | https://kotlinlang.org/docs/releases.html |
| **Node** | https://github.com/nodejs/node/releases | https://nodejs.org/en/about/previous-releases |
| **PHP** | https://github.com/php/php-src/releases | https://www.php.net/supported-versions.php |
| **Python** | https://www.python.org/downloads/ | https://github.com/python/cpython/tags |
| **Ruby** | https://github.com/ruby/ruby/releases | https://www.ruby-lang.org/en/downloads/releases/ |
| **Swift** | https://www.swift.org/install/ | https://github.com/swiftlang/swift/releases |

## Process

### Phase 1: Initial Research (Web Search)

Use web searches to gather initial version information and context. This provides:
- Recent release announcements
- Changelog highlights
- Upcoming version information
- General context about the runtime

**Search query pattern:** `"<runtime> latest version release <current_year>"`

**Example searches:**
```
Bun runtime latest version release 2026
Go golang latest version release 2026
.NET dotnet latest version release 2026
```

**Important:** Treat all information from Phase 1 as UNVERIFIED. Do not use these versions in the final output without Phase 2 verification.

### Phase 2: Direct Source Verification (REQUIRED)

For EACH runtime, fetch the authoritative source directly using WebFetch. This is the critical step that ensures accuracy.

#### Verification Commands

Run these WebFetch calls to verify each runtime:

**JavaScript/TypeScript Runtimes:**
- `WebFetch: https://github.com/oven-sh/bun/releases` - Prompt: "What is the latest release version number and date?"
- `WebFetch: https://github.com/denoland/deno/releases` - Prompt: "What is the latest release version number and date?"
- `WebFetch: https://github.com/nodejs/node/releases` - Prompt: "What is the latest release version number and date?"

**Systems Languages:**
- `WebFetch: https://go.dev/dl/` - Prompt: "What is the latest stable Go version available for download?"
- `WebFetch: https://github.com/ruby/ruby/releases` - Prompt: "What is the latest release version number and date?"
- `WebFetch: https://github.com/swiftlang/swift/releases` - Prompt: "What is the latest release version number and date?"

**JVM Languages:**
- `WebFetch: https://www.oracle.com/java/technologies/downloads/` - Prompt: "What is the latest Java JDK version available for download?"
- `WebFetch: https://github.com/JetBrains/kotlin/releases` - Prompt: "What is the latest stable release version number and date?"

**Web/Scripting Languages:**
- `WebFetch: https://github.com/php/php-src/releases` - Prompt: "What is the latest release version number and date?"
- `WebFetch: https://www.python.org/downloads/` - Prompt: "What is the latest stable Python version available for download?"

**Mobile/Cross-Platform:**
- `WebFetch: https://docs.flutter.dev/release/release-notes` - Prompt: "What is the latest stable Flutter version?"
- `WebFetch: https://dart.dev/get-dart/archive` - Prompt: "What is the latest stable Dart SDK version listed?"

**Framework/Platform:**
- `WebFetch: https://dotnet.microsoft.com/en-us/download/dotnet` - Prompt: "What is the latest .NET version available?"

**Standards (not runtime):**
- `WebFetch: https://isocpp.org/std/status` - Prompt: "What is the current C++ standard and what is the upcoming standard?"

### Phase 3: Cross-Reference Discrepancies

If Phase 1 (web search) and Phase 2 (direct fetch) show different versions:

1. **Always trust Phase 2** (direct source)
2. Document the discrepancy for awareness
3. The web search version is likely cached/outdated

**Common discrepancy patterns:**
- Patch versions differ (e.g., 1.3.7 vs 1.3.8) - releases happened between search index updates
- Minor versions differ - search results are significantly outdated
- Major versions differ - search results are severely outdated, investigate further

### Phase 4: Compile Results

Create the output with:

1. **Summary table** - Quick reference with version, date, and notes
2. **Detailed sections** - Per-runtime information including:
   - Exact version number
   - Release date (if available)
   - Key highlights/features
   - Upcoming releases
   - Link to authoritative source
3. **Verification sources table** - List all primary sources for future reference

## Output Template

```markdown
# Latest Runtime Versions

*Last updated: <date>*

*Verified from official GitHub releases and download pages*

| Runtime | Latest Version | Release Date | Notes |
|---------|---------------|--------------|-------|
| **Bun** | x.x.x | <date> | <notes> |
...

---

## Detailed Information

### <Runtime>
- **Version:** x.x.x
- **Release Date:** <date>
- **Highlights:** <features>
- **Upcoming:** <next version info>
- **Source:** [<source name>](<url>)

...

---

## Verification Sources

| Runtime | Primary Source |
|---------|---------------|
| Bun | https://github.com/oven-sh/bun/releases |
...
```

## Troubleshooting

### GitHub says "There aren't any releases here"

Some projects don't use GitHub Releases. Try:
1. Check the `/tags` page instead of `/releases`
2. Use the official website download page
3. Check the project's CHANGELOG.md file

**Known projects without GitHub Releases:**
- Go (golang/go) - Use https://go.dev/dl/
- Python (python/cpython) - Use https://www.python.org/downloads/
- Dart (dart-lang/sdk) - Use https://dart.dev/get-dart/archive
- Flutter - Use https://docs.flutter.dev/release/release-notes

### Version shows as "calculating" or dynamic

Some pages load versions via JavaScript. Try:
1. Check the archive/downloads page instead
2. Look for version mentioned in page footer ("reflects version X.X.X")
3. Use the GitHub releases/tags as backup

### Multiple version tracks (LTS vs Current)

Document both when relevant:
- **Current/Latest:** The newest version with latest features
- **LTS (Long Term Support):** Stable version recommended for production

Example runtimes with dual tracks:
- Node.js (Current + LTS)
- .NET (STS + LTS)
- Java (Latest + LTS)

## Execution Notes

- **Parallel execution:** Phase 1 searches and Phase 2 fetches can run in parallel batches (7 at a time is reasonable)
- **Freshness:** Runtime versions can change daily for active projects. Always re-verify if accuracy is critical.
- **C++ is special:** C++ is a standard, not a runtime. Report the current ISO standard (e.g., C++23) and upcoming standard (e.g., C++26).
- **Patch versions matter:** Don't round versions. 8.5.1 is different from 8.5.0.
