export async function getContextBody(ctx: any) {
  return await ctx.request.body.arrayBuffer();
}
