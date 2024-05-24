package handler

import (
	"openruntimes/types"
	"os"
)

func Main(Context *types.Context) types.ResponseOutput {
	action := Context.Req.Headers["x-action"]

	switch a := action; a {
	case "plaintextResponse":
		return Context.Res.Send("Hello World 👋", 200, nil)
	case "jsonResponse":
		return Context.Res.Json(map[string]interface{}{
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
	case "requestUrl":
		return Context.Res.Json(map[string]interface{}{
			"url":         Context.Req.Url,
			"port":        Context.Req.Port,
			"path":        Context.Req.Path,
			"query":       Context.Req.Query,
			"queryString": Context.Req.QueryString,
			"scheme":      Context.Req.Scheme,
			"host":        Context.Req.Host,
		}, 200, nil)
	case "requestHeaders":
		return Context.Res.Json(Context.Req.Headers, 200, nil)
	case "requestBodyPlaintext":
		body := Context.Req.Body.(string)
		return Context.Res.Send(body, 200, nil)
	case "requestBodyJson":
		switch Context.Req.Body.(type) {
		case string:
			return Context.Res.Json(map[string]interface{}{
				"key1": "Missing key",
				"key2": "Missing key",
				"raw":  Context.Req.BodyRaw,
			}, 200, nil)
		default:
			body := Context.Req.Body.(map[string]interface{})

			key1, ok := body["key1"]
			if !ok {
				key1 = "Missing key"
			}

			key2, ok := body["key2"]
			if !ok {
				key2 = "Missing key"
			}

			return Context.Res.Json(map[string]interface{}{
				"key1": key1,
				"key2": key2,
				"raw":  Context.Req.BodyRaw,
			}, 200, nil)
		}
	case "envVars":
		var emptyVar *string
		varValue := os.Getenv("NOT_DEFINED_VAR")
		if varValue != "" {
			emptyVar = &varValue
		}

		return Context.Res.Json(map[string]interface{}{
			"var":      os.Getenv("CUSTOM_ENV_VAR"),
			"emptyVar": emptyVar,
		}, 200, nil)
	default:
		Context.Error("Unknown action in tests.go")
		return Context.Res.Send("", 500, nil)
	}
}

/*
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
