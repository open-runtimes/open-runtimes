// Prepares what SSR bundles ship from the build root: package.json and
// node_modules. Only node_modules is shipped, so links in it must stay inside.
import {
  copyFileSync,
  existsSync,
  lstatSync,
  mkdirSync,
  readdirSync,
  readFileSync,
  readlinkSync,
  realpathSync,
  renameSync,
  rmSync,
  symlinkSync,
  writeFileSync,
} from "node:fs";
import { dirname, isAbsolute, join, relative, resolve, sep } from "node:path";

const buildDir = "/usr/local/build";
const rootModules = join(buildDir, "node_modules");

// Package manager metadata and stores. Generated dirs such as .prisma move.
const metadata = [
  ".bin",
  ".pnpm",
  ".bun",
  ".modules.yaml",
  ".package-lock.json",
  ".yarn-state.yml",
  ".yarn-integrity",
  ".cache",
  ".vite",
];

// The runtime servers bring their own copies of these (Deno maps them too)
const runtime = Object.keys(
  readJson("/usr/local/server/package.json").dependencies || {},
);

const runtimeFields = [
  "dependencies",
  "optionalDependencies",
  "peerDependencies",
];

function readJson(file) {
  try {
    return JSON.parse(readFileSync(file, "utf8"));
  } catch {
    return {};
  }
}

function names(manifest, fields) {
  return fields.flatMap((field) => Object.keys(manifest[field] || {}));
}

function exists(file) {
  try {
    lstatSync(file);
    return true;
  } catch {
    return false;
  }
}

function isLink(file) {
  try {
    return lstatSync(file).isSymbolicLink();
  } catch {
    return false;
  }
}

function realpath(file) {
  try {
    return realpathSync(file);
  } catch {
    return null;
  }
}

function inside(file, dir) {
  return file.startsWith(dir + sep);
}

// Package names in a node_modules directory, scoped ones as "@scope/name"
function packages(dir) {
  let entries;
  try {
    entries = readdirSync(dir);
  } catch {
    return [];
  }
  return entries.flatMap((name) => {
    const file = join(dir, name);
    if (!name.startsWith("@") || isLink(file)) return [name];
    try {
      return readdirSync(file).map((child) => `${name}/${child}`);
    } catch {
      return [];
    }
  });
}

// Relative, so it still resolves once node_modules moves
function link(file, target) {
  rmSync(file, { recursive: true, force: true });
  mkdirSync(dirname(file), { recursive: true });
  symlinkSync(relative(dirname(file), target), file);
}

// Copies a directory, keeping its links as they are
function copy(source, destination) {
  const stat = lstatSync(source);
  if (stat.isSymbolicLink()) {
    symlinkSync(readlinkSync(source), destination);
  } else if (stat.isDirectory()) {
    mkdirSync(destination, { recursive: true });
    for (const name of readdirSync(source)) {
      copy(join(source, name), join(destination, name));
    }
  } else {
    copyFileSync(source, destination);
  }
}

// The app is the nearest package.json above the output directory. In
// monorepos it is a subfolder of the build root.
const output = process.env.OPEN_RUNTIMES_OUTPUT_DIRECTORY || ".";
let project = resolve(buildDir, dirname(output));
if (!inside(project, buildDir)) project = buildDir;
while (project !== buildDir && !existsSync(join(project, "package.json"))) {
  project = dirname(project);
}

if (project !== buildDir) {
  // The app's manifest ("type", dependencies) is the one to ship. Root
  // dependencies stay listed, as the pack hook traces every dependency.
  const manifest = readJson(join(project, "package.json"));
  manifest.dependencies = {
    ...readJson(join(buildDir, "package.json")).dependencies,
    ...manifest.dependencies,
  };
  writeFileSync(
    join(buildDir, "package.json"),
    JSON.stringify(manifest, null, 2),
  );

  // The app was built against its own node_modules, so its entries replace
  // root entries of the same name
  const modules = join(project, "node_modules");
  for (const name of packages(modules)) {
    if (metadata.includes(name)) continue;
    const entry = join(modules, name);
    const root = join(rootModules, name);
    const target = realpath(entry);
    if (!target || realpath(root) === target) continue;
    rmSync(root, { recursive: true, force: true });
    mkdirSync(dirname(root), { recursive: true });
    if (isLink(entry)) {
      // Made relative or copied below
      symlinkSync(target, root);
    } else {
      renameSync(entry, root);
    }
  }
}

// Absolute links into the store (pnpm, bun isolated) become relative
for (const name of packages(rootModules)) {
  const entry = join(rootModules, name);
  if (!isLink(entry) || !isAbsolute(readlinkSync(entry))) continue;
  const target = realpath(entry);
  if (target && inside(target, rootModules)) link(entry, target);
}

// Workspace packages live outside node_modules. Copy the ones the app uses,
// directly or through other workspace packages. devDependencies count for the
// app itself, as configs (next.config) load them at runtime.
const pending = names(readJson(join(buildDir, "package.json")), [
  ...runtimeFields,
  "devDependencies",
]);
const copies = new Map();

function copyWorkspace(entry, source, ancestors) {
  rmSync(entry, { recursive: true, force: true });
  copy(source, entry);
  const needed = names(readJson(join(source, "package.json")), runtimeFields);
  const modules = join(entry, "node_modules");
  for (const name of packages(modules)) {
    const file = join(modules, name);
    if (!isLink(file)) continue;
    const dependency = realpath(join(source, "node_modules", name));
    rmSync(file, { force: true });
    if (!dependency) continue;
    if (inside(dependency, rootModules)) {
      link(file, dependency);
      continue;
    }
    // Another workspace package, only needed when it is a runtime dependency.
    // It resolves from its copy at the root, or from a copy nested here when
    // the root has a different package of that name.
    if (!needed.includes(name) || ancestors.includes(dependency)) continue;
    const root = join(rootModules, name);
    if (!exists(root)) {
      mkdirSync(dirname(root), { recursive: true });
      symlinkSync(dependency, root);
    } else if (
      realpath(root) !== dependency &&
      copies.get(dependency) !== root
    ) {
      copyWorkspace(file, dependency, [...ancestors, dependency]);
    }
  }
  pending.push(...needed);
}

for (const name of pending) {
  const entry = join(rootModules, name);
  const source = isLink(entry) && realpath(entry);
  // Inside node_modules already, the build root (a workspace root package) or the app
  if (
    !source ||
    inside(source, rootModules) ||
    inside(rootModules, source) ||
    source === project
  ) {
    continue;
  }
  copies.set(source, entry);
  copyWorkspace(entry, source, [source]);
}

// Absolute links left over point at workspace packages the app does not use
for (const name of packages(rootModules)) {
  const entry = join(rootModules, name);
  if (isLink(entry) && isAbsolute(readlinkSync(entry))) rmSync(entry);
}

// pnpm and bun (isolated linker) link only direct dependencies at the top level
// and hoist the rest into a private node_modules inside their store. Runtime
// servers and Nitro output import framework dependencies (@remix-run/express,
// vue-bundle-renderer) directly, so link those too.
for (const store of [".pnpm", ".bun"]) {
  const hoisted = join(rootModules, store, "node_modules");
  for (const name of packages(hoisted)) {
    const entry = join(hoisted, name);
    const target = realpath(entry);
    if (name.startsWith(".") || runtime.includes(name)) continue;
    // Hoisted workspace packages point outside node_modules
    if (
      exists(join(rootModules, name)) ||
      !target ||
      !inside(target, rootModules)
    )
      continue;
    link(join(rootModules, name), entry);
  }
}
