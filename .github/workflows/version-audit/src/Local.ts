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
        const build = data[runtime]?.build;
        const versionConfig = build?.versions?.[version];
        const base = versionConfig?.base;
        if (!base) {
          throw new Error(
            `Failed to find base image for ${runtime} ${version} in ci/runtimes.toml`,
          );
        }

        // Report under the on-disk directory layout: runtime_dir/version_dir
        // override the runtime/version names when a runtime builds into a
        // different path (e.g. python-ml lives at python/ml-<version>).
        const dir = build?.runtime_dir ?? runtime;
        const versionDir = versionConfig?.version_dir ?? version;
        const key = `${dir}/${versionDir}`;

        dockerVersions[key] = base;
      }
    }

    return dockerVersions;
  }
}
