import { DockerHub } from "./DockerHub";

export async function getNewerVersion(
  currentUpdatedAt: string,
  currentDigest: string,
  currentName: string,

  namespace: string,

  search: string,
  suffix: string,
): Promise<string> {
  const {
    lastUpdated: latestUpdatedAt,
    name: latestName,
    digest: latestDigest,
  } = await DockerHub.getLatestTag(namespace, search, suffix);

  const digestMatching = currentDigest === latestDigest;

  if (digestMatching) {
    return "";
  }

  const currentSemantics = currentName.split("-")[0];
  const latestSemantics = latestName.split("-")[0];

  try {
    const compareResult = Bun.semver.order(currentSemantics, latestSemantics);
    // Current is greater or equal
    if (compareResult === 1 || compareResult === 0) {
      return "";
    }
  } catch (_err) {}

  const _isOlder = new Date(currentUpdatedAt) < new Date(latestUpdatedAt);

  return latestName;
}

export function areDatesSame(
  date1: string,
  date2: string,
  tolerance = 60 * 5,
): boolean {
  const timeDiff = Math.abs(
    new Date(date1).getTime() - new Date(date2).getTime(),
  );
  return timeDiff > tolerance;
}
