const DOCKER_HUB_API_URL = "https://hub.docker.com/v2";
export const DOCKER_HUB_DEFAULT_NAMESPACE = "library";

export class DockerHub {
  static isValid(image: string): boolean {
    const [namespace, _] = image.split(":", 2);
    return namespace.includes(".") ? false : true;
  }

  static normalizeNamespace(namespace: string): string {
    if (!namespace.includes("/")) {
      return DOCKER_HUB_DEFAULT_NAMESPACE + "/" + namespace;
    }
    return namespace;
  }

  static async getLatestTag(
    image: string,
    version: string,
    suffix: string,
  ): Promise<{
    name: string;
    lastUpdated: string;
    digest: string;
  }> {
    const parts = image.split("/", 2);
    const tag = parts.pop();
    const namespace = parts.join("/");

    async function findTag(page: number = 1) {
      if (page > 5) {
        console.warn(`⚠️ Page ${page} is pretty high for ${image}:${version}`);
      }

      const path =
        DOCKER_HUB_API_URL +
        `/namespaces/${namespace}/repositories/${tag}/tags?name=${version}&ordering=last_updated&page_size=100&page=${page}`;
      const response = await fetch(path);
      const json = await response.json();

      if (json.message) {
        throw new Error(json.message);
      }

      const results = json.results;

      const result = results.find((r: any) => {
        const matchesSuffix = r.name.includes(suffix);

        const nameWithoutSuffix = suffix ? r.name.split(suffix)[0] : r.name;
        const hasSemanticVersion = nameWithoutSuffix.split(".").length >= 3;

        const isNotRc =
          !r.name.toLowerCase().includes("rc") &&
          !r.name.toLowerCase().includes("beta");

        const isValid = matchesSuffix && hasSemanticVersion && isNotRc;
        return isValid;
      });

      if (result) {
        return result;
      }

      if (json.next) {
        return await findTag(page + 1);
      }

      return null;
    }

    const result = await findTag();

    if (!result) {
      throw new Error(`No tag found for ${image} ${version} ${suffix}`);
    }

    const name = result.name;
    const lastUpdated = result.last_updated;
    const digest = result.digest;

    return { name, lastUpdated, digest };
  }

  static async getTagHash(
    image: string,
    version: string,
  ): Promise<{
    name: string;
    lastUpdated: string;
    digest: string;
  }> {
    const parts = image.split("/");
    const tag = parts.pop();
    const namespace = parts.join("/");

    const path =
      DOCKER_HUB_API_URL +
      `/namespaces/${namespace}/repositories/${tag}/tags/${version}`;
    const response = await fetch(path);
    const json = await response.json();

    if (json.message) {
      throw new Error(json.message);
    }

    const name = json.name;
    const lastUpdated = json.last_updated;
    const digest = json.digest;

    return { name, lastUpdated, digest };
  }
}
