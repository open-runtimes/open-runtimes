const fetch = require("node-fetch");

module.exports = async (context) => {
    const action = context.req.headers['X-Action'];

    switch (action) {
        case 'plaintextResponse':
            return context.res.send('Hello World 👋');
        case 'jsonResponse':
            return context.res.json({ json: true, message: 'Developers are awesome.' });
        case 'redirectResponse':
            return context.res.redirect('https://github.com/');
        case 'emptyResponse':
            return context.res.empty();
        case 'noResponse':
            context.res.send('This should be ignored, as it is not returned.');
            break;
        case 'doubleResponse':
            context.res.send('This should be ignored.');
            return context.res.send('This should be returned.');
        case 'headersResponse':
            return context.res.send('OK', 200, {
                'first-header': 'first-value',
                'second-header': context.req.headers['X-Open-Runtimes-Custom-In-Header'] ?? 'missing',
                'x-open-runtimes-custom-out-header': 'third-value'
            });
        case 'statusResponse':
            return context.res.send('FAIL', 404);
        case 'requestMethod':
            return context.res.send(context.req.method);
        case 'requestUrl':
            return context.res.send(context.req.url);
        case 'requestHeaders':
            return context.res.json(context.req.headers);
        case 'requestBodyPlaintext':
            return context.res.send(context.req.body);
        case 'requestBodyJson':
            return context.res.json({
                key1: context.req.body.key1 ?? 'Missing key',
                key2: context.req.body.key2 ?? 'Missing key',
                raw: context.req.rawBody
            })
        case 'envVars':
            return context.res.json({
                var: process.env.CUSTOM_ENV_VAR,
                emptyVar: process.env.NOT_DEFINED_VAR ?? null
            });
        case 'logs':
            console.log('Native log');
            context.log('Debug log');
            context.error('Error log');
            
            context.log(42);
            context.log(4.2);
            context.log(true);

            return context.res.send('');
        default:
            throw new Error('Unkonwn action');
    }
}

