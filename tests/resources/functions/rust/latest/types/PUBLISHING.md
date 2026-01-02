# Publishing types-for-rust to crates.io

This guide explains how to publish the `openruntimes` crate to crates.io.

## Prerequisites

1. **crates.io Account**: Create an account at [crates.io](https://crates.io/)
2. **API Token**: Generate an API token from your [crates.io account settings](https://crates.io/me)
3. **Cargo**: Ensure you have Rust and Cargo installed

## Setup

### 1. Login to crates.io

```bash
cargo login <your-api-token>
```

This will store your credentials in `~/.cargo/credentials.toml`.

### 2. Update Package Metadata

Before publishing, ensure `Cargo.toml` has the correct metadata:

```toml
[package]
name = "openruntimes"
version = "1.0.0"  # Follow semver
edition = "2021"
authors = ["Open Runtimes <team@openruntimes.io>"]
description = "Types for Rust Open Runtime"
license = "MIT"
repository = "https://github.com/open-runtimes/types-for-rust"
keywords = ["openruntimes", "serverless", "runtime"]
categories = ["web-programming"]
readme = "README.md"  # Create if needed
```

### 3. Verify the Package

Run checks before publishing:

```bash
cd types-for-rust

# Check for errors
cargo check

# Run tests (if any)
cargo test

# Build the package
cargo build

# Verify package contents
cargo package --list

# Dry run publish
cargo publish --dry-run
```

## Publishing

### 1. First Release

```bash
# Publish to crates.io
cargo publish
```

### 2. Subsequent Releases

For each new release:

1. **Update version** in `Cargo.toml`:
   ```toml
   version = "1.0.1"  # Increment following semver
   ```

2. **Commit changes**:
   ```bash
   git add Cargo.toml
   git commit -m "Bump version to 1.0.1"
   ```

3. **Create git tag**:
   ```bash
   git tag -a v1.0.1 -m "Release v1.0.1"
   git push origin v1.0.1
   ```

4. **Publish**:
   ```bash
   cargo publish
   ```

## Versioning (Semantic Versioning)

Follow [SemVer](https://semver.org/):

- **MAJOR** (1.0.0 → 2.0.0): Breaking API changes
- **MINOR** (1.0.0 → 1.1.0): New backwards-compatible features
- **PATCH** (1.0.0 → 1.0.1): Backwards-compatible bug fixes

## Usage in Runtime

Once published, update `runtimes/rust/versions/latest/Cargo.toml`:

```toml
[dependencies]
openruntimes = "1.0"  # Use version range
```

For development/testing with local path:

```toml
[dependencies]
openruntimes = { path = "/path/to/types-for-rust" }
```

Or use in workspace:

```toml
[dependencies]
openruntimes = { path = "../../../../types-for-rust" }
```

## Yanking a Release

If you need to remove a published version:

```bash
cargo yank --vers 1.0.0
```

To un-yank:

```bash
cargo yank --vers 1.0.0 --undo
```

## Troubleshooting

### Error: "the remote server responded with an error: crate name is already taken"

The `openruntimes` name might be taken. Try:
- `open-runtimes`
- `openruntimes-types`
- Contact crates.io to request the name transfer

### Error: "failed to verify package tarball"

Run `cargo clean` and try again.

### Error: "some files are not in the published crate"

Add files to the package:

```toml
[package]
include = [
    "src/**/*",
    "Cargo.toml",
    "README.md",
    "LICENSE",
]
```

## Resources

- [Cargo Book - Publishing on crates.io](https://doc.rust-lang.org/cargo/reference/publishing.html)
- [crates.io Policies](https://crates.io/policies)
- [Rust API Guidelines](https://rust-lang.github.io/api-guidelines/)
