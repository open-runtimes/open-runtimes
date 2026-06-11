// @ts-ignore
import data from "../../../../ci/runtimes.toml";

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

      for (const version of versions) {
        // Base images live in the [<runtime>.build.versions] tables of
        // ci/runtimes.toml (the source docker-bake.json is generated from)
        const base = data[runtime]?.build?.versions?.[version]?.base;
        if (!base) {
          throw new Error(
            `Failed to find base image for ${runtime} ${version} in ci/runtimes.toml`,
          );
        }

        // Edge case due to "-" in name: report under the directory layout name
        const key =
          runtime === "python-ml"
            ? `python/ml-${version}`
            : `${runtime}/${version}`;

        dockerVersions[key] = base;
      }
    }

    return dockerVersions;
  }
}
