import { nodeFileTrace } from "@vercel/nft";
import { writeFileSync, existsSync, realpathSync } from "node:fs";
import { resolve, relative } from "node:path";

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

// Node 20.19+ and 22.12+ also match "module-sync", older runtimes do not
const fileList = new Set();
try {
  for (const conditions of [["node"], ["node", "module-sync"]]) {
    const result = await nodeFileTrace([absoluteEntry], {
      base: outputDir,
      processCwd: outputDir,
      conditions,
    });
    for (const file of result.fileList) fileList.add(file);
  }
} catch (err) {
  console.error(`Trace failed: ${err.message}`);
  process.exit(2);
}

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
