namespace DotNetRuntime;

using System;
using System.Collections;
using System.Text.Json;

public class Handler {
    static readonly HttpClient http = new();

    public async Task<RuntimeOutput> Main(RuntimeContext context)
    {
        var Action = context.Req.Headers.TryGetValue("x-action", out string ActionValue) ? ActionValue : "";

        switch (Action)
        {
            case "plaintextResponse":
                return context.Res.Text("Hello World 👋");
            case "jsonResponse":
                return context.Res.Json(new()
                {
                    { "json", true },
                    { "message", "Developers are awesome." }
                });
            case "customCharsetResponse":
                return context.Res.Text("ÅÆ", 200, new Dictionary<string, string>()
                {
                    { "content-type", "text/plain; charset=iso-8859-1" }
                });
            case "uppercaseCharsetResponse":
                return context.Res.Text("ÅÆ", 200, new Dictionary<string, string>()
                {
                    { "content-type", "TEXT/PLAIN" }
                });
            case "multipartResponse":
                return context.Res.Text(@"--12345
Content-Disposition: form-data; name=""partOne""

Why just have one part?
--12345
Content-Disposition: form-data; name=""partTwo""

When you can have two!
--12345--", 200, new Dictionary<string, string>()
                {
                    { "content-type", "multipart/form-data; boundary=12345" }
                });
            case "redirectResponse":
                return context.Res.Redirect("https://github.com/");
            case "emptyResponse":
                return context.Res.Empty();
            case "noResponse":
                context.Res.Text("This should be ignored, as it is not returned.");

                // Simulate test data. Return nessessary in Java
                context.Error("Return statement missing. return context.Res.Empty() if no response is expected.");
                return context.Res.Text("", 500);
            case "doubleResponse":
                context.Res.Text("This should be ignored.");
                return context.Res.Text("This should be returned.");
            case "headersResponse":
                var headers = new Dictionary<string, string>();
                headers.Add("first-header", "first-value");
                var SecondHeader = context.Req.Headers.TryGetValue("x-open-runtimes-custom-in-header", out string secondHeaderValue) ? secondHeaderValue : "missing";
                headers.Add("second-header", SecondHeader);
                var cookieHeader = context.Req.Headers.TryGetValue("cookie", out string cookieHeaderValue) ? cookieHeaderValue : "missing";
                headers.Add("cookie", cookieHeader);
                headers.Add("x-open-runtimes-custom-out-header", "third-value");
                return context.Res.Text("OK", 200, headers);
            case "statusResponse":
                return context.Res.Text("FAIL", 404);
            case "requestMethod":
                return context.Res.Text(context.Req.Method);
            case "requestUrl":
                return context.Res.Json(new()
                {
                    { "url", context.Req.Url },
                    { "port", context.Req.Port },
                    { "path", context.Req.Path },
                    { "query", context.Req.Query },
                    { "queryString", context.Req.QueryString },
                    { "scheme", context.Req.Scheme },
                    { "host", context.Req.Host }
                });
            case "enforcedHeaders":
                var xCustom = context.Req.Headers.TryGetValue("x-custom", out string xCustomValue) ? xCustomValue : "";
                var xCustomUppercase = context.Req.Headers.TryGetValue("x-custom-uppercase", out string xCustomUppercaseValue) ? xCustomUppercaseValue : "";
                var xOpenRuntimesCustom = context.Req.Headers.TryGetValue("x-open-runtimes-custom", out string xOpenRuntimesCustomValue) ? xOpenRuntimesCustomValue : "";

                return context.Res.Json(new()
                {
                    { "x-custom", xCustom },
                    { "x-custom-uppercase", xCustomUppercase },
                    { "x-open-runtimes-custom", xOpenRuntimesCustom },
                });
            case "requestHeaders":
                var json = new Dictionary<string, object>();

                foreach (var entry in context.Req.Headers)
                {
                    json.Add(entry.Key, entry.Value);
                }

                return context.Res.Json(json);
            case "requestBodyText":
                return context.Res.Text(context.Req.BodyText);
            case "requestBodyJson":
                return context.Res.Json(context.Req.BodyJson);
            case "requestBodyBinary":
                return context.Res.Binary(context.Req.BodyBinary);
            case "requestBodyTextAuto":
                return context.Res.Text((string) context.Req.Body);
            case "requestBodyJsonAuto":
                return context.Res.Json((Dictionary<string, object>) context.Req.Body);
            case "binaryResponse1":
                byte[] bytes1 = new byte[] {0, 10, 255};
                return context.Res.Binary(bytes1); // byte[]
            case "binaryResponse2":
                byte[] bytes2 = new byte[] {0, 20, 255};
                return context.Res.Binary(bytes2); // Just a filler
            case "binaryResponse3":
                byte[] bytes3 = new byte[] {0, 30, 255};
                return context.Res.Binary(bytes3); // Just a filler
            case "binaryResponse4":
                byte[] bytes4 = new byte[] {0, 40, 255};
                return context.Res.Binary(bytes4); // Just a filler
            case "binaryResponse5":
                byte[] bytes5 = new byte[] {0, 50, 255};
                return context.Res.Binary(bytes5); // Just a filler
            case "binaryResponseLarge":
                byte[] bytes = context.Req.BodyBinary;
                byte[] hash;
                using (var md5 = System.Security.Cryptography.MD5.Create()) {
                    md5.TransformFinalBlock(bytes, 0, bytes.Length);
                    hash = md5.Hash;
                }
                string hex = BitConverter.ToString(hash).Replace("-", "").ToLower();
                return context.Res.Send(hex, 200, new() {
                    {"x-method", context.Req.Method}
                });
            case "envVars":
                return context.Res.Json(new()
                {
                    { "var", Environment.GetEnvironmentVariable("CUSTOM_ENV_VAR") ?? null },
                    { "emptyVar", Environment.GetEnvironmentVariable("NOT_DEFINED_VAR") ?? null },
                });
            case "logs":
                Console.WriteLine("Native log");
                context.Log("Debug log");
                context.Error("Error log");

                context.Log("Log+With+Plus+Symbol");

                context.Log(42);
                context.Log(4.2);
                context.Log(true);

                var Obj = new Dictionary<string, string>();
                Obj.Add("objectKey", "objectValue");

                context.Log(Obj);

                var Arr = new List<string>();
                Arr.Add("arrayValue");

                context.Log(Arr);

                return context.Res.Text("");
            case "library":
                var response = await http.GetStringAsync($"https://jsonplaceholder.typicode.com/todos/{context.Req.BodyRaw}");
                var todo = JsonSerializer.Deserialize<Dictionary<string, object>>(response) ?? new Dictionary<string, object>();

                return context.Res.Json(new()
                {
                    { "todo", todo }
                });
            case "timeout":
                context.Log("Timeout start.");

                await Task.Delay(3000);

                context.Log("Timeout end.");
                return context.Res.Text("Successful response.");
            case "deprecatedMethods":
                return context.Res.Send(context.Req.BodyRaw);
            case "deprecatedMethodsUntypedBody":
                return context.Res.Send("50"); // Send only supported String
            default:
                throw new Exception("Unknown action");
        }
    }
}
