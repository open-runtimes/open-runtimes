import { nodeFileTrace } from "@vercel/nft";
import { writeFileSync, existsSync, realpathSync } from "node:fs";
import { resolve, relative } from "node:path";
import { createRequire } from "node:module";

const entrypoint = process.argv[2];
const outputDir = resolve(process.argv[3]);

if (!entrypoint || !outputDir) {
  console.error("Usage: nft.mjs <entrypoint> <output-dir>");
  process.exit(1);
}

const absoluteEntry = resolve(outputDir, entrypoint);

if (!existsSync(absoluteEntry)) {
  console.error(`Entrypoint not found: ${absoluteEntry}`);
  process.exit(1);
}

let result;
try {
  result = await nodeFileTrace([absoluteEntry], {
    base: outputDir,
    processCwd: outputDir,
  });
} catch (err) {
  console.error(`Trace failed: ${err.message}`);
  process.exit(2);
}

const { fileList } = result;

// Collect files: resolve symlinks so targets are included too
const files = new Set();

for (const f of fileList) {
  if (f.startsWith("/") || f.startsWith("..")) continue;
  files.add(f);

  // If it's a symlink, also include the real path
  const abs = resolve(outputDir, f);
  try {
    const real = realpathSync(abs);
    const rel = relative(outputDir, real);
    if (!rel.startsWith("/") && !rel.startsWith("..")) {
      files.add(rel);
    }
  } catch {
    // File may not exist on disk (e.g. optional dependency)
  }
}

const manifestPath = resolve(outputDir, ".nft-files");
writeFileSync(manifestPath, [...files].join("\0"), "utf-8");

// Log per-package summary
const packages = new Set();
for (const f of files) {
  const m = f.match(/^node_modules\/(?:@[^/]+\/)?[^/]+/);
  if (m) packages.add(m[0]);
}

console.log(
  `Traced ${files.size} files across ${packages.size} packages (${fileList.size} total)`,
);
