const fetch = require("node-fetch");

module.exports = async (context) => {
    const action = context.req.headers['x-action'];

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
                'second-header': context.req.headers['x-open-runtimes-custom-in-header'] ?? 'missing',
                'x-open-runtimes-custom-out-header': 'third-value'
            });
        case 'statusResponse':
            return context.res.send('FAIL', 404);
        case 'requestMethod':
            return context.res.send(context.req.method);
        case 'requestUrl':
            return context.res.json({
                url: context.req.url,
                port: context.req.port,
                path: context.req.path,
                query: context.req.query,
                queryString: context.req.queryString,
                scheme: context.req.scheme,
                host: context.req.host
            });
        case 'requestHeaders':
            return context.res.json(context.req.headers);
        case 'requestBodyPlaintext':
            return context.res.send(context.req.body);
        case 'requestBodyJson':
            return context.res.json({
                key1: context.req.body.key1 ?? 'Missing key',
                key2: context.req.body.key2 ?? 'Missing key',
                raw: context.req.bodyRaw
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

            context.log({ objectKey: 'objectValue' });
            context.log([ 'arrayValue' ]);

            return context.res.send('');
        case 'library':
            const todo = await fetch(`https://jsonplaceholder.typicode.com/todos/${context.req.bodyRaw}`).then(r => r.json());
            return context.res.json({ todo });
        case 'timeout':
            context.log('Timeout start.');

            await new Promise((resolve) => {
                setTimeout(resolve, 3000);
            });

            context.log('Timeout end.');
            return context.res.send('Successful response.');
        default:
            throw new Error('Unknown action');
    }
}

