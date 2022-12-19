import axiod from "https://deno.land/x/axiod/mod.ts";

/*
    'req' variable has:
        'headers' - object with request headers
        'payload' - object with request body data
        'variables' - object with function variables
    'res' variable has:
        'send(text, status)' - function to return text response. Status code defaults to 200
        'json(obj, status)' - function to return JSON response. Status code defaults to 200
    
    If an error is thrown, a response with code 500 will be returned.
*/

export default async function(req: any, res: any) {
    const payload = JSON.parse(req.payload  === '' ? '{}' : req.payload);

    if(payload.noResponse) {
        console.log("Exit with no response.");
        return;
    }

    const todo = (await axiod.get(`https://jsonplaceholder.typicode.com/todos/${payload.id ?? 1}`)).data;
    console.log('log');
    console.warn('warning');
    console.error('error');
    console.info('info');
    console.debug('debug');
    console.log('log1', 'log2');
    console.log({hello: 'world'});
    console.log(['hello', 'world']);
    res.json({
        isTest: true,
        message: 'Hello Open Runtimes 👋',
        header: req.headers['x-test-header'],
        variable: req.variables['test-variable'],
        todo
    });
}