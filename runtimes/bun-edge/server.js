/*
const USER_CODE_PATH = '/usr/code-start';

export default {
  port: 3000,
  async fetch(request) {
    const body = await request.json();

    if (request.headers.get("x-internal-challenge") !== process.env.INTERNAL_RUNTIME_KEY) {
      return new Response("Unauthorized", {
        status: 500
      });
    }

    const functionRequest = {
      env: body.env ?? {},
      headers: body.headers ?? {},
      payload: body.payload ?? ''
    };

    let response;

    const functionResponse = {
      send: (text, status = 200) => {
        response = new Response(text, {
          status
        });
      },
      json: (json, status = 200) => {3
        response = new Response(json, {
          status
        });
      }
    };
    
    try {
      const userFunction = (await import(USER_CODE_PATH + '/' + process.env.INTERNAL_RUNTIME_ENTRYPOINT)).default;
  
      if (!(userFunction || userFunction.constructor || userFunction.call || userFunction.apply)) {
        throw new Error("User function is not valid.")
      }
  
      await userFunction(functionRequest, functionResponse);
      return response;
    } catch (error) {
      return new Response(error.message.includes("Cannot resolve module") ? 'Code file not found.' : error.stack || error.message, {
        status: 500
      });
    }
  },
};
*/

export default {
  port: 3000,
  fetch(request) {
    return new Response("Welcome to Bun!");
  },
};