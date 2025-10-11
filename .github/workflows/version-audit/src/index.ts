// @ts-ignore
import data from "../../../../ci/runtimes.toml";
import { join } from "path";
import { readFileSync } from "fs";
import { DOCKER_HUB_DEFAULT_NAMESPACE, DockerHub } from "./DockerHub";

async function main() {
  const runtimeVersions: {
    [runtimeName: string]: string; // latest version
  } = {};

  for (const entries of Object.entries(data)) {
    const runtimeName = entries[0];
    const config: any = entries[1];

    if (config.runtime) {
      console.info(`🔵 Skipping ${runtimeName}`);
      continue;
    }

    runtimeVersions[runtimeName] = config.versions[0];
  }

  const dockerVersions: {
    [runtimeName: string]: string; // Docker FROM image
  } = {};

  for (let [runtimeName, latestVersion] of Object.entries(runtimeVersions)) {
    if (runtimeName === "python-ml") {
      runtimeName = "python";
      latestVersion = "ml-" + latestVersion;
    }

    const dockerfilePath = join(
      process.cwd(),
      "..",
      "..",
      "..",
      "runtimes",
      runtimeName,
      "versions",
      latestVersion,
      "Dockerfile",
    );

    const dockerVersion = extractDockerVersion(dockerfilePath);
    dockerVersions[runtimeName] = dockerVersion;
  }

  for (let [runtimeName, dockerImage] of Object.entries(dockerVersions)) {
    let [namespace, version] = dockerImage.split(":", 2);

    if (namespace.includes(".")) {
      console.info(
        `🔴 Skipping ${dockerImage} because it's not a Docker Hub image`,
      );
      continue;
    }

    if (!namespace.includes("/")) {
      namespace = DOCKER_HUB_DEFAULT_NAMESPACE + "/" + namespace;
    }

    const [semantics, suffix] = version.split("-", 2);

    const [semanticMajor, semanticMinor, semanticPatch] = semantics.split(
      ".",
      3,
    );

    const currentName = `${semanticMajor}.${semanticMinor}.${semanticPatch}`;
    const { lastUpdated: currentUpdatedAt } = await DockerHub.getTagHash(
      namespace,
      currentName + "-" + suffix,
    );

    if (semanticPatch) {
      const { lastUpdated: latestUpdatedAt, name: latestName } =
        await DockerHub.getLatestTag(
          namespace,
          `${semanticMajor}.${semanticMinor}.`,
        );

      const timeDiff = Math.abs(
        new Date(currentUpdatedAt).getTime() -
          new Date(latestUpdatedAt).getTime(),
      );

      if (timeDiff > 60 * 5) {
        // 5 minutes
        console.info(
          `🟠 [${runtimeName}] Patch update available from ${currentName} to ${latestName}`,
        );
      } else {
        console.info(`🟢 [${runtimeName}] Patch update is up to date`);
      }
    }

    if (semanticMinor) {
      const { lastUpdated: latestUpdatedAt, name: latestName } =
        await DockerHub.getLatestTag(namespace, `${semanticMajor}.`);

      const timeDiff = Math.abs(
        new Date(currentUpdatedAt).getTime() -
          new Date(latestUpdatedAt).getTime(),
      );

      if (timeDiff > 60 * 5) {
        // 5 minutes
        console.info(
          `🟠 [${runtimeName}] Minor update available from ${currentName} to ${latestName}`,
        );
      } else {
        console.info(`🟢 [${runtimeName}] Minor update is up to date`);
      }
    }

    if (semanticMajor) {
      const { lastUpdated: latestUpdatedAt, name: latestName } =
        await DockerHub.getLatestTag(namespace, "latest");

      const timeDiff = Math.abs(
        new Date(currentUpdatedAt).getTime() -
          new Date(latestUpdatedAt).getTime(),
      );

      if (timeDiff > 60 * 5) {
        // 5 minutes
        console.info(
          `🟠 [${runtimeName}] Major update available from ${currentName} to ${latestName}`,
        );
      } else {
        console.info(`🟢 [${runtimeName}] Major update is up to date`);
      }
    }
  }

  // TODO: Send Discord webhook, or submit issue, or similar, for all versions requiring attention
}

function extractDockerVersion(dockerfilePath: string): string {
  const content = readFileSync(dockerfilePath, "utf-8");
  const fromMatch = content.match(/^FROM\s+([^\s]+)/m);

  if (fromMatch) {
    return fromMatch[1];
  }

  return "";
}

main();
