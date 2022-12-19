import axiod from "https://deno.land/x/axiod/mod.ts";
import { Application } from "https://deno.land/x/oak@v10.5.1/mod.ts";
import * as path from "https://deno.land/std@0.138.0/path/mod.ts";
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

    const todo = (await axiod.get(`https://jsonplaceholder.typicode.com/todos/${payload.id ?? 1}`)).data;
    
    console.log('String');
    console.log(42);
    console.log(4.2);
    console.log(true);

    console.log('String2');
    console.info('String3');
    console.debug('String4');
    console.warn('String5');

    res.json({
        isTest: true,
        message: 'Hello Open Runtimes 👋',
        header: req.headers['x-test-header'],
        variable: req.variables['test-variable'],
        todo
    });
}