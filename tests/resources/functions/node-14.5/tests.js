const fetch = require("node-fetch");

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

module.exports = async (req, res) => {
    const payload = JSON.parse(req.payload === '' ? '{}' : req.payload);

    const todo = await fetch(`https://jsonplaceholder.typicode.com/todos/${payload.id ?? 1}`).then(r => r.json());

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
