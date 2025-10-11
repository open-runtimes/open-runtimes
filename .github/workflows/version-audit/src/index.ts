// @ts-ignore
import data from "../../../../ci/runtimes.toml";
import { join } from "path";
import { readFileSync } from "fs";

async function main() {
  const runtimeVersions: {
    [runtimeName: string]: string; // latest version
  } = {};

  for (const entries of Object.entries(data)) {
    const runtimeName = entries[0];
    const config: any = entries[1];

    if (config.runtime) {
      console.info(
        `Skipping runtime "${runtimeName}" because it's not main runtime`,
      );
      continue;
    }

    runtimeVersions[runtimeName] = config.versions[0];
  }

  console.log("Runtime latest versions:");
  console.log(JSON.stringify(runtimeVersions, null, 2));

  const dockerVersions: {
    [runtimeName: string]: string; // Docker FROM image
  } = {};

  for (let [runtimeName, latestVersion] of Object.entries(runtimeVersions)) {
    if(runtimeName === "python-ml") {
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
    console.log(dockerfilePath);

    const dockerVersion = extractDockerVersion(dockerfilePath);
    dockerVersions[runtimeName] = dockerVersion;
  }

  console.log("\nRuntime exact versions:");
  console.log(JSON.stringify(dockerVersions, null, 2));
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
