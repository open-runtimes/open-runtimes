const fetch = require("node-fetch");

module.exports = async (context) => {
    const action = context.req.headers['x-action'];

    switch (action) {
        case 'plaintextResponse':
            return context.res.text('Hello World 👋');
        case 'jsonResponse':
            return context.res.json({ json: true, message: 'Developers are awesome.' });
        case 'customCharsetResponse':
            return context.res.text('ÅÆ', 200, { 'content-type': 'text/plain; charset=iso-8859-1' });
        case 'multipartResponse':
            return context.res.text(`--12345
Content-Disposition: form-data; name="partOne"

Why just have one part?
--12345
Content-Disposition: form-data; name="partTwo"

When you can have two!
--12345--`, 200, {'content-type': 'multipart/form-data; boundary=12345'});
        case 'redirectResponse':
            return context.res.redirect('https://github.com/');
        case 'emptyResponse':
            return context.res.empty();
        case 'noResponse':
            context.res.text('This should be ignored, as it is not returned.');
            break;
        case 'doubleResponse':
            context.res.text('This should be ignored.');
            return context.res.text('This should be returned.');
        case 'headersResponse':
            return context.res.text('OK', 200, {
                'first-header': 'first-value',
                'second-header': context.req.headers['x-open-runtimes-custom-in-header'] ?? 'missing',
                'cookie': context.req.headers['cookie'] ?? 'missing',
                'x-open-runtimes-custom-out-header': 'third-value'
            });
        case 'statusResponse':
            return context.res.text('FAIL', 404);
        case 'requestMethod':
            return context.res.text(context.req.method);
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
        case 'requestBodyText':
            return context.res.text(context.req.bodyText);
        case 'requestBodyJson':
            return context.res.json(context.req.bodyJson);
        case 'requestBodyBinary':
            return context.res.binary(context.req.bodyBinary);
        case 'requestBodyTextAuto':
            return context.res.text(context.req.body);
        case 'requestBodyJsonAuto':
            return context.res.json(context.req.body);
        case 'requestBodyBinaryAuto':
            return context.res.binary(context.req.body);
        case 'envVars':
            return context.res.json({
                var: process.env.CUSTOM_ENV_VAR,
                emptyVar: process.env.NOT_DEFINED_VAR ?? null
            });
        case 'logs':
            console.log('Native log');
            context.log('Debug log');
            context.error('Error log');
                
            context.log("Log+With+Plus+Symbol");
            
            context.log(42);
            context.log(4.2);
            context.log(true);

            context.log({ objectKey: 'objectValue' });
            context.log([ 'arrayValue' ]);

            return context.res.text('');
        case 'library':
            const todo = await fetch(`https://jsonplaceholder.typicode.com/todos/${context.req.bodyRaw}`).then(r => r.json());
            return context.res.json({ todo });
        case 'timeout':
            context.log('Timeout start.');

            await new Promise((resolve) => {
                setTimeout(resolve, 3000);
            });

            context.log('Timeout end.');
            return context.res.text('Successful response.');
        case 'deprecatedMethods':
            return context.res.send(context.req.bodyRaw);
        case 'responseChunkedSimple':
            context.res.start();
            context.res.writeText('OK1');
            context.res.writeText('OK2');
            return context.res.end();
        case 'responseChunkedComplex':
            context.res.start(201, { 'x-start-header': 'start' });
            context.res.writeText('Start');
            await new Promise((resolve) => {
                setTimeout(resolve, 1000);
            });
            context.res.writeText('Step1');
            await new Promise((resolve) => {
                setTimeout(resolve, 1000);
            });
            context.res.writeJson({ step2: true });
            await new Promise((resolve) => {
                setTimeout(resolve, 1000);
            });
            context.res.writeBinary(Buffer.from("0123456789abcdef", "hex"));
            return context.res.end({ 'x-trainer-header': 'end' });
        case 'responseChunkedErrorStartDouble':
            context.res.start();
            context.res.start();
            context.res.writeText('OK');
            return context.res.end();
        case 'responseChunkedErrorStartMissing':
            context.res.writeText('OK');
            return context.res.end();
        case 'responseChunkedErrorStartWriteMissing':
            return context.res.end();
        default:
            throw new Error('Unknown action');
    }
}

