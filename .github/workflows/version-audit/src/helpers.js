import { join } from "path";
import { readFileSync } from "fs";
import { DOCKER_HUB_DEFAULT_NAMESPACE, DockerHub } from "./DockerHub";

export function getLatestVersions(runtimesData) {
  const runtimeVersions = {};

  for (const [runtimeName, config] of Object.entries(runtimesData)) {
    if (config.runtime) {
      console.info(`🔵 Skipping ${runtimeName}`);
      continue;
    }

    runtimeVersions[runtimeName] = config.versions[0];
  }

  return runtimeVersions;
}

export function getDockerImages(runtimeVersions) {
  const dockerVersions = {};

  for (let [runtimeName, latestVersion] of Object.entries(runtimeVersions)) {
    const adjusted = adjustRuntime(runtimeName, latestVersion);
    const dockerfilePath = buildDockerfilePath(adjusted.name, adjusted.version);
    const dockerImage = extractFromDockerfile(dockerfilePath);
    
    dockerVersions[adjusted.name] = dockerImage;
  }

  return dockerVersions;
}

export function adjustRuntime(runtimeName, version) {
  if (runtimeName === "python-ml") {
    return {
      name: "python",
      version: "ml-" + version
    };
  }
  
  return {
    name: runtimeName,
    version: version
  };
}

export function buildDockerfilePath(runtimeName, version) {
  return join(
    process.cwd(),
    "..",
    "..",
    "..",
    "runtimes",
    runtimeName,
    "versions",
    version,
    "Dockerfile",
  );
}

export function extractFromDockerfile(dockerfilePath) {
  const content = readFileSync(dockerfilePath, "utf-8");
  const fromMatch = content.match(/^FROM\s+([^\s]+)/m);

  if (fromMatch) {
    return fromMatch[1];
  }

  return "";
}

export function isNonDockerHub(dockerImage) {
  const [namespace] = dockerImage.split(":", 2);
  return namespace.includes(".");
}

export function normalizeNamespace(namespace) {
  if (!namespace.includes("/")) {
    return DOCKER_HUB_DEFAULT_NAMESPACE + "/" + namespace;
  }
  return namespace;
}

export function parseVersion(dockerImage) {
  const [namespace, version] = dockerImage.split(":", 2);
  const normalizedNamespace = normalizeNamespace(namespace);
  
  const [semantics, suffix] = version.split("-", 2);
  const [major, minor, patch] = semantics.split(".", 3);

  return {
    namespace: normalizedNamespace,
    suffix,
    major,
    minor,
    patch,
    current: `${major}.${minor}.${patch}`
  };
}

export async function getCurrentInfo(namespace, current, suffix) {
  const tagName = suffix ? `${current}-${suffix}` : current;
  return await DockerHub.getTagHash(namespace, tagName);
}

export async function checkPatch(runtimeName, namespace, major, minor, currentUpdated, current) {
  const { lastUpdated: latest, name: latestName } = 
    await DockerHub.getLatestTag(namespace, `${major}.${minor}.`);

  if (hasUpdate(currentUpdated, latest)) {
    console.info(`🟠 [${runtimeName}] Patch update available from ${current} to ${latestName}`);
  } else {
    console.info(`🟢 [${runtimeName}] Patch update is up to date`);
  }
}

export async function checkMinor(runtimeName, namespace, major, currentUpdated, current) {
  const { lastUpdated: latest, name: latestName } = 
    await DockerHub.getLatestTag(namespace, `${major}.`);

  if (hasUpdate(currentUpdated, latest)) {
    console.info(`🟠 [${runtimeName}] Minor update available from ${current} to ${latestName}`);
  } else {
    console.info(`🟢 [${runtimeName}] Minor update is up to date`);
  }
}

export async function checkMajor(runtimeName, namespace, currentUpdated, current) {
  const { lastUpdated: latest, name: latestName } = 
    await DockerHub.getLatestTag(namespace, "latest");

  if (hasUpdate(currentUpdated, latest)) {
    console.info(`🟠 [${runtimeName}] Major update available from ${current} to ${latestName}`);
  } else {
    console.info(`🟢 [${runtimeName}] Major update is up to date`);
  }
}

export function hasUpdate(currentUpdated, latestUpdated) {
  const timeDiff = Math.abs(
    new Date(currentUpdated).getTime() - new Date(latestUpdated).getTime()
  );
  
  return timeDiff > 60 * 5;
}