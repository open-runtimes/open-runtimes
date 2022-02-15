import { Application } from "https://deno.land/x/oak@v10.1.0/mod.ts";
import * as path from "https://deno.land/std@0.119.0/path/mod.ts";

const DEFAULT_PATH = '/usr/code';
const DEFAULT_FILE = 'mod.ts';

const app = new Application();

app.use(async (ctx) => {
  const { value } = ctx.request.body({ type: 'json' });
  const body = await value;

  if (ctx.request.headers.get("x-internal-challenge") !== Deno.env.get("INTERNAL_RUNTIME_KEY")) {
    ctx.response.status = 401;
    ctx.response.body = {
      code: 401,
      message: "Unauthorized"
    };
    return;
  }

  const request = {
    env: body.env ?? {},
    headers: body.headers ?? {},
    payload: body.payload ?? {}
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
    const userFunction = (await import(path.join(body.path ?? DEFAULT_PATH, body.file ?? DEFAULT_FILE))).default;

    if (!(userFunction || userFunction.constructor || userFunction.call || userFunction.apply)) {
      throw new Error("User function is not valid.")
    }

    await userFunction(request, response);
  } catch (error) {
    ctx.response.status = 500;
    ctx.response.body = {
      code: 500,
      message: error.message.includes("Cannot resolve module") ? 'Code file not found.' : error.stack || error.message
    };
  }
});

await app.listen({ port: 3000 });