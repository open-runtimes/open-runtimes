import { DockerHub } from "./DockerHub";

export async function getNewerVersion(
  currentUpdatedAt: string,
  currentDigest: string,

  namespace: string,

  search: string,
  suffix: string,
): Promise<string> {
  const {
    lastUpdated: latestUpdatedAt,
    name: latestName,
    digest: latestDigest,
  } = await DockerHub.getLatestTag(namespace, search, suffix);

  const _datesMatching = areDatesSame(currentUpdatedAt, latestUpdatedAt);
  const digestMatching = currentDigest === latestDigest;

  return digestMatching ? "" : latestName;
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
