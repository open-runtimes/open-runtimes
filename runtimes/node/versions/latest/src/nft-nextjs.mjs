// Reads null-delimited .nft.json paths from stdin, writes null-delimited
// node_modules/ relative paths to stdout.
const fs = require("fs");
const path = require("path");

const base = process.argv[1];
let buf = "";
process.stdin.on("data", (d) => (buf += d));
process.stdin.on("end", () => {
  const seen = new Set();
  for (const nj of buf.split("\0").filter(Boolean)) {
    try {
      const { files } = JSON.parse(fs.readFileSync(nj, "utf-8"));
      const dir = path.dirname(nj);
      for (const f of files || []) {
        const rel = path.relative(base, path.resolve(dir, f));
        if (
          rel.startsWith("node_modules/") &&
          !rel.startsWith("..") &&
          !seen.has(rel)
        ) {
          seen.add(rel);
          process.stdout.write(rel + "\0");
        }
      }
    } catch {}
  }
});
