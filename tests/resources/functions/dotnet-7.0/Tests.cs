using System;
using System.Net.Http;
using System.Collections;
using System.Text.Json;
using System.Threading.Tasks;

namespace DotNetRuntime {
    public class Handler {
        static readonly HttpClient http = new HttpClient();

        public async Task<RuntimeOutput> Main(RuntimeContext context)
        {
            var Action = context.Req.Headers.TryGetValue("x-action", out string ActionValue) ? ActionValue : "";

            switch (Action)
            {
                case "plaintextResponse":
                    return context.Res.Text("Hello World 👋");
                case "jsonResponse":
                    return context.Res.Json(new Dictionary<string, object?>()
                    {
                        { "json", true },
                        { "message", "Developers are awesome." }
                    });
                case "customCharsetResponse":
                    return context.Res.Text("ÅÆ", 200, new Dictionary<string, string>()
                    {
                        { "content-type", "text/plain; charset=iso-8859-1" }
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
                    return context.Res.Json(new Dictionary<string, object?>()
                    {
                        { "url", context.Req.Url },
                        { "port", context.Req.Port },
                        { "path", context.Req.Path },
                        { "query", context.Req.Query },
                        { "queryString", context.Req.QueryString },
                        { "scheme", context.Req.Scheme },
                        { "host", context.Req.Host }
                    });
                case "requestHeaders":
                    var json = new Dictionary<string, object>();

                    foreach (var entry in context.Req.Headers)
                    {
                        json.Add(entry.Key, entry.Value);
                    }

                    return context.Res.Json(json);
                case "requestBodyText":
                    return context.Res.Text((string) context.Req.Body);
                case "requestBodyJson":
                    var key1 = "";
                    var key2 = "";

                    if(context.Req.Body is string) {
                        key1 = "Missing key";
                        key2 = "Missing key";
                    } else {
                        var body = (Dictionary<String, Object>) context.Req.Body;

                        key1 = body.TryGetValue("key1", out var key1Value) ? key1Value.ToString() : "Missing key";
                        key2 = body.TryGetValue("key2", out var key2Value) ? key2Value.ToString() : "Missing key";
                    }

                    return context.Res.Json(new Dictionary<string, object?>()
                    {
                        { "key1", key1 },
                        { "key2", key2 },
                        { "raw", context.Req.BodyRaw }
                    });
                case "envVars":
                    return context.Res.Json(new Dictionary<string, object?>()
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

                    return context.Res.Json(new Dictionary<string, object?>()
                    {
                        { "todo", todo }
                    });
                case "timeout":
                    context.Log("Timeout start.");

                    await Task.Delay(3000);

                    context.Log("Timeout end.");
                    return context.Res.Text("Successful response.");
                default:
                    throw new Exception("Unknown action");
            }
        }
    }
}
