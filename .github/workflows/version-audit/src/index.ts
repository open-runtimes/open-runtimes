import { getNewerVersion } from "./helpers.ts";

import { DockerHub } from "./DockerHub.ts";
import { Semantics } from "./Semantics.ts";
import { Local } from "./Local.ts";

async function main() {
  const runtimesVersions = Local.getRuntimesVersions();
  const runtimeImages = Local.getDockerImages(runtimesVersions);

  // Name of runtimes that already had full scan
  // Prevents re-runing same checks
  let fullScans: string[] = [];

  for (const entries of Object.entries(runtimeImages)) {
    const runtime: string = entries[0] ?? "";
    const image: string = entries[1] ?? "";

    const fullScan = fullScans.includes(runtime.split("/")[0]);
    if (!fullScan) {
      fullScans.push(runtime.split("/")[0]);
    }

    if (!DockerHub.isValid(image)) {
      console.info(`🔴 Skipping ${image} because it's not a Docker Hub image`);
      continue;
    }

    const { namespace, prefix, suffix, major, minor, patch, tag } =
      Semantics.parseVersion(image);

    const {
      lastUpdated: currentUpdatedAt,
      digest: currentDigest,
      name: currentName,
    } = await DockerHub.getTagHash(namespace, tag);

    // Major version scan (run only once)
    if (!fullScan && major) {
      const search = `${prefix}`;
      const updatedName = await getNewerVersion(
        currentUpdatedAt,
        currentDigest,
        currentName,
        namespace,
        search,
        suffix,
      );

      if (updatedName) {
        console.info(
          `🟣 ${runtime} major version update is available (${tag} -> ${updatedName})`,
        );
      } else {
        console.info(`🟢 ${runtime} major version is up to date (${tag})`);
      }
    }

    // Minor version scan (run once, and when OPR doesnt specify minor)
    if ((!fullScan && minor) || (runtime.split(".").length < 2 && minor)) {
      const search = `${prefix}${major}.`;
      const updatedName = await getNewerVersion(
        currentUpdatedAt,
        currentDigest,
        currentName,
        namespace,
        search,
        suffix,
      );

      if (updatedName) {
        console.info(
          `🟠 ${runtime} minor version update is available (${tag} -> ${updatedName})`,
        );
      } else {
        console.info(`🟢 ${runtime} minor version is up to date (${tag})`);
      }
    }

    // Patch version scan (run always)
    if (patch) {
      const search = `${prefix}${major}.${minor}.`;
      const updatedName = await getNewerVersion(
        currentUpdatedAt,
        currentDigest,
        currentName,
        namespace,
        search,
        suffix,
      );

      if (updatedName) {
        console.info(
          `🟠 ${runtime} patch version update is available (${tag} -> ${updatedName})`,
        );
      } else {
        console.info(`🟢 ${runtime} patch version is up to date (${tag})`);
      }
    }
  }
}

main();
