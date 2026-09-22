export async function getContextBody(ctx: any) {
  return await ctx.request.body({
    type: "bytes",
    limit: 20 * 1024 * 1024,
  }).value;
}
