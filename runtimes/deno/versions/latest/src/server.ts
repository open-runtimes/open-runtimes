import { Logger } from "./logger.ts";
import { action } from "./action.ts";
import { createApp } from "./app.ts";

const app = createApp();

app.addEventListener("listen", () => {
  console.log(`HTTP server successfully started!`);
});

app.use(async (ctx: any) => {
  const logger = new Logger(
    ctx.request.headers.get("x-open-runtimes-logging"),
    ctx.request.headers.get("x-open-runtimes-log-id"),
  );
  await logger.setup();

  try {
    await action(logger, ctx);
  } catch (e) {
    logger.write([e], Logger.TYPE_ERROR);

    ctx.response.headers.set("x-open-runtimes-log-id", logger.id);
    await logger.end();

    ctx.response.status = 500;
    ctx.response.body = "";
  }
});

await app.listen({ port: 3000 });
