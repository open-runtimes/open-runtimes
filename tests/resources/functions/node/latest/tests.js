const fetch = require("node-fetch");
const crypto = require('crypto');

module.exports = async (context) => {
    const action = context.req.headers['x-action'];

    switch (action) {
        case 'plaintextResponse':
            return context.res.text('Hello World 👋');
        case 'jsonResponse':
            return context.res.json({ json: true, message: 'Developers are awesome.' });
        case 'customCharsetResponse':
            return context.res.text('ÅÆ', 200, { 'content-type': 'text/plain; charset=iso-8859-1' });
        case 'uppercaseCharsetResponse':
            return context.res.text('ÅÆ', 200, { 'content-type': 'TEXT/PLAIN' });
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
        case "enforcedHeaders":
            return context.res.json({
                "x-custom": context.req.headers["x-custom"],
                "x-custom-uppercase": context.req.headers["x-custom-uppercase"],
                "x-open-runtimes-custom": context.req.headers["x-open-runtimes-custom"],
            });
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
        case 'binaryResponse1':
            return context.res.binary(Buffer.from((Uint8Array.from([0, 10, 255])))); // Buffer
        case 'binaryResponse2':
            return context.res.binary(Buffer.from((Uint8Array.from([0, 20, 255])))); // Just a filler
        case 'binaryResponse3':
            return context.res.binary(Buffer.from((Uint8Array.from([0, 30, 255])))); // Just a filler
        case 'binaryResponse4':
            return context.res.binary(Buffer.from((Uint8Array.from([0, 40, 255])))); // Just a filler
        case 'binaryResponse5':
            return context.res.binary(Buffer.from((Uint8Array.from([0, 50, 255])))); // Just a filler
        case 'binaryResponseLarge':
            const buffer = Buffer.from(context.req.bodyBinary);
            const hash = crypto.createHash('md5').update(buffer).digest('hex');
            return context.res.send(hash, 200, {
                'x-method': context.req.method
            });
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
        case 'deprecatedMethodsUntypedBody':
            return context.res.send(50);
        case 'deprecatedMethodsBytesBody':
            // Buffer from base64. It's content as MD5 is d3a119080678e92f8a0d7e2547b46291
            const image = Buffer.from('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', 'base64');

            return context.res.send(image, 200, {
                'content-type': 'image/png'
            });
        default:
            throw new Error('Unknown action');
    }
}

