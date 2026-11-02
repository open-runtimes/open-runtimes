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
  unlinkSync,
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
const runtimeFields = [
  "dependencies",
  "optionalDependencies",
  "peerDependencies",
];

function readJson(file) {
  return existsSync(file) ? JSON.parse(readFileSync(file, "utf8")) : {};
}

function lstat(file) {
  try {
    return lstatSync(file);
  } catch {
    return null;
  }
}

function realpath(file) {
  try {
    return realpathSync(file);
  } catch {
    return null;
  }
}

// Package names in a node_modules directory, scoped ones as "@scope/name"
function packages(dir) {
  if (!lstat(dir)?.isDirectory()) return [];
  return readdirSync(dir).flatMap((name) => {
    const file = join(dir, name);
    if (!name.startsWith("@") || !lstat(file).isDirectory()) return [name];
    return readdirSync(file).map((child) => `${name}/${child}`);
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

// Copies a workspace package into node_modules and returns the names it needs
// at runtime. Its links into node_modules become relative. A workspace
// dependency resolves from its copy at the root, or from a copy nested here
// when the root has a different package of that name.
function copyWorkspace(entry, source, copies) {
  rmSync(entry, { recursive: true, force: true });
  copy(source, entry);
  const manifest = readJson(join(source, "package.json"));
  const needed = runtimeFields.flatMap((field) =>
    Object.keys(manifest[field] || {}),
  );
  for (const name of packages(join(entry, "node_modules"))) {
    const file = join(entry, "node_modules", name);
    if (!lstat(file).isSymbolicLink()) continue;
    const dependency = realpath(join(source, "node_modules", name));
    unlinkSync(file);
    if (!dependency) continue;
    if (dependency.startsWith(rootModules + sep)) {
      link(file, dependency);
      continue;
    }
    // Development-only workspace packages are not shipped
    if (!needed.includes(name)) continue;
    const root = join(rootModules, name);
    const copied = copies.get(dependency);
    if (!lstat(root)) {
      mkdirSync(dirname(root), { recursive: true });
      symlinkSync(dependency, root);
    } else if (copied) {
      // One instance, as in the install; a parent copy resolves on its own
      if (copied !== root && !file.startsWith(copied + sep)) link(file, copied);
    } else if (realpath(root) !== dependency) {
      copies.set(dependency, file);
      needed.push(...copyWorkspace(file, dependency, copies));
    }
  }
  return needed;
}

// The app is the nearest package.json above the output directory. In
// monorepos it is a subfolder of the build root.
const output = process.env.OPEN_RUNTIMES_OUTPUT_DIRECTORY || ".";
let project = resolve(buildDir, dirname(output));
if (!project.startsWith(buildDir + sep)) project = buildDir;
while (project !== buildDir && !existsSync(join(project, "package.json"))) {
  project = dirname(project);
}

if (project !== buildDir) {
  // The app's manifest ("type", dependencies) is the one to ship. Root
  // dependencies stay listed, as the pack hook traces every dependency.
  const manifest = readJson(join(project, "package.json"));
  const root = readJson(join(buildDir, "package.json"));
  manifest.dependencies = { ...root.dependencies, ...manifest.dependencies };
  writeFileSync(
    join(buildDir, "package.json"),
    JSON.stringify(manifest, null, 2),
  );

  // The app was built against its own node_modules, so its entries replace
  // root entries of the same name. Links are made relative or copied below.
  for (const name of packages(join(project, "node_modules"))) {
    if (metadata.includes(name)) continue;
    const entry = join(project, "node_modules", name);
    const target = realpath(entry);
    const rootEntry = join(rootModules, name);
    if (!target || realpath(rootEntry) === target) continue;
    rmSync(rootEntry, { recursive: true, force: true });
    mkdirSync(dirname(rootEntry), { recursive: true });
    if (lstat(entry).isSymbolicLink()) {
      symlinkSync(target, rootEntry);
      continue;
    }
    try {
      renameSync(entry, rootEntry);
    } catch (error) {
      // node_modules on another mount
      if (error.code !== "EXDEV") throw error;
      copy(entry, rootEntry);
      rmSync(entry, { recursive: true, force: true });
    }
  }
}

// Workspace packages live outside node_modules. Copy the ones the app uses,
// directly or through other workspace packages. devDependencies count for the
// app itself, as configs (next.config) load them at runtime.
const manifest = readJson(join(buildDir, "package.json"));
const pending = [...runtimeFields, "devDependencies"].flatMap((field) =>
  Object.keys(manifest[field] || {}),
);
const copies = new Map();
for (const name of pending) {
  const entry = join(rootModules, name);
  const source = lstat(entry)?.isSymbolicLink() && realpath(entry);
  // Skip the store, the build root (a workspace root package) and the app
  if (
    !source ||
    source.startsWith(rootModules + sep) ||
    rootModules.startsWith(source + sep) ||
    source === project
  ) {
    continue;
  }
  copies.set(source, entry);
  pending.push(...copyWorkspace(entry, source, copies));
}

// Absolute links left: those into the store become relative, the rest point
// at workspace packages the app does not use
for (const name of packages(rootModules)) {
  const entry = join(rootModules, name);
  if (!lstat(entry).isSymbolicLink() || !isAbsolute(readlinkSync(entry))) {
    continue;
  }
  const target = realpath(entry);
  if (target?.startsWith(rootModules + sep)) link(entry, target);
  else unlinkSync(entry);
}

// pnpm and bun (isolated linker) link only direct dependencies at the top level
// and hoist the rest into a private node_modules inside their store. Runtime
// servers and Nitro output import framework dependencies (@remix-run/express,
// vue-bundle-renderer) directly, so link those too, except the ones the
// runtime servers bring themselves (Deno maps them in src/ssr/deno.json).
const runtime = Object.keys(
  readJson("/usr/local/server/package.json").dependencies || {},
);
for (const store of [".pnpm", ".bun"]) {
  const hoisted = join(rootModules, store, "node_modules");
  for (const name of packages(hoisted)) {
    const target = realpath(join(hoisted, name));
    const rootEntry = join(rootModules, name);
    // Hoisted workspace packages point outside node_modules
    const outside = !target?.startsWith(rootModules + sep);
    if (name.startsWith(".") || runtime.includes(name) || outside) continue;
    if (!lstat(rootEntry)) link(rootEntry, join(hoisted, name));
  }
}
