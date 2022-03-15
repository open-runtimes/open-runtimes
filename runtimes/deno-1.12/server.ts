import { Application } from "https://deno.land/x/oak@v8.0.0/mod.ts";
import * as path from "https://deno.land/std@0.119.0/path/mod.ts";

const USER_CODE_PATH = '/usr/code-start';

const app = new Application();

app.use(async (ctx) => {
  const { value } = ctx.request.body({ type: 'json' });
  const body = await value;

  if (ctx.request.headers.get("x-internal-challenge") !== Deno.env.get("INTERNAL_RUNTIME_KEY")) {
    ctx.response.status = 500;
    ctx.response.body = "Unauthorized";
    return;
  }

  const request = {
    env: body.env ?? {},
    headers: body.headers ?? {},
    payload: body.payload ?? ''
  };

  const response = {
    send: (text: string, status = 200) => {
      ctx.response.status = status;
      ctx.response.body = text;
    },
    json: (json: Record<string, unknown>, status = 200) => {
      ctx.response.status = status;
      ctx.response.body = json;
    }
  };

  try {
    const userFunction = (await import(USER_CODE_PATH + '/' + Deno.env.get("INTERNAL_RUNTIME_ENTRYPOINT"))).default;

    if (!(userFunction || userFunction.constructor || userFunction.call || userFunction.apply)) {
      throw new Error("User function is not valid.")
    }

    await userFunction(request, response);
  } catch (error) {
    ctx.response.status = 500;
    ctx.response.body = error.message.includes("Cannot resolve module") ? 'Code file not found.' : error.stack || error.message;
  }
});

await app.listen({ port: 3000 });