import { DockerHub } from "./DockerHub.ts";

export class Semantics {
  static parseVersion(dockerImage: string): {
    namespace: string;
    prefix: string;
    suffix: string;
    major: string | undefined;
    minor: string | undefined;
    patch: string | undefined;
    tag: string;
  } {
    const [namespace, version] = dockerImage.split(":", 2);
    const normalizedNamespace = DockerHub.normalizeNamespace(namespace);

    const sections = version.split("-", 2);

    let prefixes: string[] = [];
    let suffixes: string[] = [];
    let hasMain = false;
    let major: string | undefined,
      minor: string | undefined,
      patch: string | undefined;

    for (const section of sections) {
      if (section.split(".").length >= 3) {
        // Main version section
        hasMain = true;
        [major, minor, patch] = section.split(".", 3);
      } else {
        // Prefix or suffix
        if (hasMain) {
          suffixes.push(section);
        } else {
          prefixes.push(section);
        }
      }
    }

    if (!hasMain) {
      const firstSection = sections.shift() ?? "";
      [major, minor, patch] = firstSection.split(".", 3);
      prefixes = [];
      suffixes = [];
      suffixes.push(...sections);
    }

    const response = {
      namespace: normalizedNamespace,
      suffix: (suffixes.length > 0 ? "-" : "") + suffixes.join("-"),
      prefix: prefixes.join("-") + (prefixes.length > 0 ? "-" : ""),
      major,
      minor,
      patch,
      tag: "",
    };

    const semantics = [major, minor, patch].filter(
      (item) => item !== undefined && item !== null && item !== "",
    );
    response.tag = `${response.prefix}${semantics.join(".")}${response.suffix}`;
    
    return response;
  }
}
