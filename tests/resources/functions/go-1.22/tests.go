package handler

import (
	"openruntimes/types"
)

func Main(Context *types.Context) types.ResponseOutput {
	action := Context.Req.Headers["x-action"]

	switch a := action; a {
	case "plaintextResponse":
		return Context.Res.Send("Hello World 👋", 200, nil)
	case "jsonResponse":
		return Context.Res.Json(map[string]any{
			"json":    true,
			"message": "Developers are awesome.",
		}, 200, nil)
	case "customCharsetResponse":
		return Context.Res.Send("ÅÆ", 200, map[string]string{
			"content-type": "text/plain; charset=iso-8859-1",
		})
	case "multipartResponse":
		return Context.Res.Send(`--12345
Content-Disposition: form-data; name="partOne"

Why just have one part?
--12345
Content-Disposition: form-data; name="partTwo"

When you can have two!
--12345--`, 200, map[string]string{
			"content-type": "multipart/form-data; boundary=12345",
		})
	case "redirectResponse":
		return Context.Res.Redirect("https://github.com/", 301, nil)
	case "emptyResponse":
		return Context.Res.Empty()
	case "noResponse":
		Context.Res.Send("This should be ignored, as it is not returned.", 200, nil)

		// Simulate test data. Return nessessary in Golang
		Context.Error("Return statement missing. return context.getRes().empty() if no response is expected.")
		return Context.Res.Send("", 500, nil)
	case "doubleResponse":
		Context.Res.Send("This should be ignored.", 200, nil)
		return Context.Res.Send("This should be returned.", 200, nil)
	case "headersResponse":
		secondHeader, ok := Context.Req.Headers["x-open-runtimes-custom-in-header"]
		if !ok {
			secondHeader = "missing"
		}

		cookie, ok := Context.Req.Headers["cookie"]
		if !ok {
			cookie = "missing"
		}

		return Context.Res.Send("OK", 200, map[string]string{
			"first-header":                      "first-value",
			"second-header":                     secondHeader,
			"cookie":                            cookie,
			"x-open-runtimes-custom-out-header": "third-value",
		})
	case "statusResponse":
		return Context.Res.Send("FAIL", 404, nil)
	case "requestMethod":
		return Context.Res.Send(Context.Req.Method, 200, nil)
	default:
		Context.Error("Unknown action in tests.go")
		return Context.Res.Send("", 500, nil)
	}
}

/*
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

       context.log("Log+With+Plus+Symbol");

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
*/
