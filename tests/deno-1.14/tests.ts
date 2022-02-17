import axiod from "https://deno.land/x/axiod/mod.ts";

/*
    'req' variable has:
        'headers' - object with request headers
        'payload' - object with request body data
        'env' - object with environment variables
    'res' variable has:
        'send(text, status)' - function to return text response. Status code defaults to 200
        'json(obj, status)' - function to return JSON response. Status code defaults to 200
    
    If an error is thrown, a response with code 500 will be returned.
*/

export default async function(req: any, res: any) {
    const payload = JSON.parse(req.payload);

    const todo = (await axiod.get(`https://jsonplaceholder.typicode.com/todos/${payload.id ?? 1}`)).data;

    res.json({
        isTest: true,
        message: 'Hello Open Runtimes 👋',
        header: req.headers['x-test-header'],
        env: req.env['test-env'],
        todo
    });
}