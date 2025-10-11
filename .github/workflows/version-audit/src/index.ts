// @ts-ignore
import data from "../../../../ci/runtimes.toml";
import {
  getLatestVersions,
  getDockerImages,
  isNonDockerHub,
  parseVersion,
  getCurrentInfo,
  checkPatch,
  checkMinor,
  checkMajor
} from "./helpers.js";

async function main() {
  const runtimeVersions = getLatestVersions(data);
  const dockerVersions = getDockerImages(runtimeVersions);

  for (const [runtimeName, dockerImage] of Object.entries(dockerVersions)) {
    if (isNonDockerHub(dockerImage)) {
      console.info(`🔴 Skipping ${dockerImage} because it's not a Docker Hub image`);
      continue;
    }

    const { namespace, suffix, major, minor, patch, current } = parseVersion(dockerImage);
    const { lastUpdated: currentUpdated } = await getCurrentInfo(namespace, current, suffix);

    if (patch) {
      await checkPatch(runtimeName, namespace, major, minor, currentUpdated, current);
    }

    if (minor) {
      await checkMinor(runtimeName, namespace, major, currentUpdated, current);
    }

    if (major) {
      await checkMajor(runtimeName, namespace, currentUpdated, current);
    }
  }
}

main();