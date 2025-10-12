// @ts-ignore
import data from "../../../../ci/runtimes.toml";
import { join } from "path";
import { readFileSync } from "fs";

export class Local {
  static getRuntimesVersions(): Record<string, string[]> {
    const runtimes = {};

    for (const entries of Object.entries(data)) {
      const runtimeName: string = entries[0] ?? "";
      const config: any = entries[1] ?? {};

      // Avoid non-main runtimes
      if (config.runtime) {
        console.info(`🔵 Skipping ${runtimeName}`);
        continue;
      }

      if (!runtimes[runtimeName]) {
        runtimes[runtimeName] = [];
      }

      // Remove JS duplicate version (used only for testing)
      const versions = config.versions.filter(
        (version: string) => !version.endsWith("-mjs"),
      );

      runtimes[runtimeName].push(...versions);
    }

    return runtimes;
  }

  static getDockerImages(
    runtimes: Record<string, string[]>,
  ): Record<string, string> {
    const dockerVersions = {};

    for (const entries of Object.entries(runtimes)) {
      let runtime: string = entries[0] ?? "";
      let versions: string[] = entries[1] ?? [];

      // Edge case due to "-" in name
      if (runtime === "python-ml") {
        runtime = "python";
        versions = versions.map((version) => `ml-${version}`);
      }

      for (const version of versions) {
        const dockerFilePath = join(
          process.cwd(),
          "..",
          "..",
          "..",
          "runtimes",
          runtime,
          "versions",
          version,
          "Dockerfile",
        );

        const dockerFile = readFileSync(dockerFilePath, "utf-8");

        const tagMatch = dockerFile.match(/^FROM\s+([^\s]+)/m);
        if (!tagMatch || !tagMatch[1]) {
          throw new Error(
            `Failed to extract Docker image tag from ${dockerFilePath}`,
          );
        }

        const tag = tagMatch[1];

        const key = `${runtime}/${version}`;

        if (!dockerVersions[key]) {
          dockerVersions[key] = [];
        }

        dockerVersions[key] = tag;
      }
    }

    return dockerVersions;
  }
}
