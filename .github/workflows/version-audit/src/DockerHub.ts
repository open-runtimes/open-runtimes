const DOCKER_HUB_API_URL = "https://hub.docker.com/v2";
export const DOCKER_HUB_DEFAULT_NAMESPACE = "library";

export class DockerHub {
  static async getLatestTag(image: string, version: string) {
    const parts = image.split("/");
    const tag = parts.pop();
    const namespace = parts.join("/");

    const path =
      DOCKER_HUB_API_URL +
      `/namespaces/${namespace}/repositories/${tag}/tags?name=${version}&&ordering=last_updated`;
    const response = await fetch(path);
    const json = await response.json();

    const result = json.results[0];

    const name = result.name;
    const lastUpdated = result.last_updated;

    return { name, lastUpdated };
  }

  static async getTagHash(image: string, version: string) {
    const parts = image.split("/");
    const tag = parts.pop();
    const namespace = parts.join("/");

    const path =
      DOCKER_HUB_API_URL +
      `/namespaces/${namespace}/repositories/${tag}/tags/${version}`;
    const response = await fetch(path);
    const json = await response.json();

    const name = json.name;
    const lastUpdated = json.last_updated;

    return { name, lastUpdated };
  }
}
