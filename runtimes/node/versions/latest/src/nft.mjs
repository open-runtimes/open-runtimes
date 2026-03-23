import { nodeFileTrace } from "@vercel/nft";
import { writeFileSync, existsSync, realpathSync } from "node:fs";
import { resolve, relative } from "node:path";

// Usage: nft.mjs <entrypoint...> <output-dir>
// Last argument is always the output directory; all preceding args are entrypoints.
const args = process.argv.slice(2);
const outputDir = resolve(args.pop());
const entrypoints = args;

if (!entrypoints.length || !outputDir) {
  console.error("Usage: nft.mjs <entrypoint...> <output-dir>");
  process.exit(1);
}

const absoluteEntries = [];
for (const ep of entrypoints) {
  const abs = resolve(outputDir, ep);
  if (!existsSync(abs)) {
    console.error(`Entrypoint not found: ${abs}`);
    process.exit(1);
  }
  absoluteEntries.push(abs);
}

let result;
try {
  result = await nodeFileTrace(absoluteEntries, {
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
