import { Logger } from "./logger.ts";
import { action } from "./action.ts";
import { createApp } from "./app.ts";

const app = createApp();

app.addEventListener("listen", () => {
  console.log(`HTTP server successfully started!`);
});

app.use(async (ctx: any) => {
  if (ctx.request.url.pathname === "/__opr/health") {
    ctx.response.status = 200;
    ctx.response.body = "OK";
    return;
  }
  if (ctx.request.url.pathname === "/__opr/timings") {
    const timings = await Deno.readTextFile("/mnt/telemetry/timings.txt");
    ctx.response.headers.set("content-type", "text/plain; charset=utf-8");
    ctx.response.status = 200;
    ctx.response.body = timings;
    return;
  }

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

const controller = new AbortController();
const shutdown = () => controller.abort();
Deno.addSignalListener("SIGTERM", shutdown);
Deno.addSignalListener("SIGINT", shutdown);
await app.listen({ port: 3000, signal: controller.signal });
Deno.removeSignalListener("SIGTERM", shutdown);
Deno.removeSignalListener("SIGINT", shutdown);
Deno.exit(0);
