const fetch = require("node-fetch");

export default async function(context: any) {
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
                host: context.req.host,
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
        case 'binaryResponse1':
            return context.res.binary(Uint8Array.from([0, 10, 255])); // Uint8Array
        case 'binaryResponse2':
            return context.res.binary(Buffer.from((Uint8Array.from([0, 20, 255])))); // Buffer
        case 'binaryResponse3':
            return context.res.binary(Uint8Array.from([0, 30, 255]).buffer); // ArrayBuffer
        case 'binaryResponse4':
            return context.res.binary(new Blob([Uint8Array.from([0, 40, 255])])); // Blob
        case 'binaryResponse5':
            return context.res.binary(Uint8Array.from([0, 50, 255])); // Just a filler
        case 'binaryResponseLarge':
            const buffer = Buffer.from(context.req.bodyBinary);
            const md5 = new Bun.CryptoHasher('md5');
            const hash = md5.update(buffer).digest('hex');
            return context.res.send(hash, 200, {
                'x-method': context.req.method
            });
        case 'envVars':
            return context.res.json({
                var: Bun.env["CUSTOM_ENV_VAR"] ?? null,
                emptyVar: Bun.env["NOT_DEFINED_VAR"] ?? null
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
            // Buffer from base64. It's content as MD5 is 2a8fdeea08e939e9a7c05653544a1374
            const image = Buffer.from('iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAAXNSR0IB2cksfwAAAAlwSFlzAAAsSwAALEsBpT2WqQAAAMlQTFRFAAAA/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu+zZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/Tdt/TZv/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZv/TZv/TZu/TZu/TZu/TZu/TZu/TZu/Tdv/TZu/TZuuSxTMwAAAEN0Uk5TABN71PrYkxIu5P/jNyDf60XK3vkOWv11JiVsYazyawE8zInhtgd8bXTitQaw8WcBHMbPXP7pciMWIDLd1yIx5hWA/BXEE2wAAACqSURBVHicXY7PCwFxEMXfkxVKkVDbHrb2YMVBOZM/Xzk52AO1tYpNSdRGfhTzne8qvMM085mZ1yMAiky5wQxAWUZtmSmo8QkrhyeD63J5rxZ5ldvCAWzJoSNfPL+Axhb0jmiSCeCLE2MwSOFyraYdre0M3ko9u5c/EG4U9BL5Xpp2ECsQ04BcAEObjyNG6NvseV5/D1RC5mkl+niX4kuymXD+CzDlozT7gDdmIiQgwIp6VQAAAABJRU5ErkJggg==', 'base64');

            return context.res.send(image, 200, {
                'content-type': 'image/png'
            });
        case 'spreadOperatorLogs':
            const engine = 'open-runtimes';
            context.log("engine:", engine);
            context.error("engine:", engine);
            return context.res.text('OK');
        default:
            throw new Error('Unknown action.');
    }
}
