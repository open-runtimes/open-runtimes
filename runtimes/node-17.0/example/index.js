const fetch = require("node-fetch");

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

module.exports = async (req, res) => {
    const payload = JSON.parse(req.payload === '' ? '{}' : req.payload);

    const todo = await fetch(`https://jsonplaceholder.typicode.com/todos/${payload.id ?? 1}`).then(r => r.json());

    console.log('log');
    console.warn('warning');
    console.error('error');
    console.info('info');
    console.debug('debug');
    console.log('log1', 'log2');
    console.log({hello: 'world'});
    console.log(['hello', 'world']);

    res.json({
        message: 'Hello Open Runtimes 👋',
        todo
    });
}