export async function fileStreamReady(file: Deno.FsFile) {
  await file.ready;
  return;
}
