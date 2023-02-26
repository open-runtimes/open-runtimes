namespace DotNetRuntime;

using System;
using System.Collections;
using System.Text.Json;

public class Handler {
    static readonly HttpClient http = new();

    public async Task<RuntimeOutput> Main(RuntimeContext Context)
    {
        var Action = Context.Req.Headers.TryGetValue("x-action", out string ActionValue) ? ActionValue : "";

        switch (Action)
        {
            case "plaintextResponse":
                return Context.Res.Send("Hello World 👋");
            case "jsonResponse":
                return Context.Res.Json(new()
                {
                    { "json", true },
                    { "message", "Developers are awesome." }
                });
            case "redirectResponse":
                return Context.Res.Redirect("https://github.com/");
            case "emptyResponse":
                return Context.Res.Empty();
            case "noResponse":
                Context.Res.Send("This should be ignored, as it is not returned.");

                // Simulate test data. Return nessessary in Java
                Context.Error("Return statement missing. return Context.Res.Empty() if no response is expected.");
                return Context.Res.Send("", 500);
            case "doubleResponse":
                Context.Res.Send("This should be ignored.");
                return Context.Res.Send("This should be returned.");
            case "headersResponse":
                var Headers = new Dictionary<string, string>();
                Headers.Add("first-header", "first-value");
                var SecondHeader = Context.Req.Headers.TryGetValue("x-open-runtimes-custom-in-header", out string SecondHeaderValue) ? SecondHeaderValue : "missing";
                Headers.Add("second-header", SecondHeader);
                Headers.Add("x-open-runtimes-custom-out-header", "third-value");
                return Context.Res.Send("OK", 200, Headers);
            case "statusResponse":
                return Context.Res.Send("FAIL", 404);
            case "requestMethod":
                return Context.Res.Send(Context.Req.Method);
            case "requestUrl":
                return Context.Res.Json(new()
                {
                    { "url", Context.Req.Url },
                    { "port", Context.Req.Port },
                    { "path", Context.Req.Path },
                    { "query", Context.Req.Query },
                    { "queryString", Context.Req.QueryString },
                    { "scheme", Context.Req.Scheme },
                    { "host", Context.Req.Host }
                });
            case "requestHeaders":
                var Json = new Dictionary<string, object>();

                foreach (var Entry in Context.Req.Headers)
                {
                    Json.Add(Entry.Key, Entry.Value);
                }

                return Context.Res.Json(Json);
            case "requestBodyPlaintext":
                return Context.Res.Send((string) Context.Req.Body);
            case "requestBodyJson":
                var Key1 = "";
                var Key2 = "";

                if(Context.Req.Body is string) {
                    Key1 = "Missing key";
                    Key2 = "Missing key";
                } else {
                    var Body = (Dictionary<String, Object>) Context.Req.Body;

                    Key1 = Body.TryGetValue("key1", out var Key1Value) ? Key1Value.ToString() : "Missing key";
                    Key2 = Body.TryGetValue("key2", out var Key2Value) ? Key2Value.ToString() : "Missing key";
                }

                return Context.Res.Json(new()
                {
                    { "key1", Key1 },
                    { "key2", Key2 },
                    { "raw", Context.Req.BodyString }
                });
            case "envVars":
                return Context.Res.Json(new()
                {
                    { "var", Environment.GetEnvironmentVariable("CUSTOM_ENV_VAR") ?? null },
                    { "emptyVar", Environment.GetEnvironmentVariable("NOT_DEFINED_VAR") ?? null },
                });
            case "logs":
                Console.WriteLine("Native log");
                Context.Log("Debug log");
                Context.Error("Error log");
                
                Context.Log(42);
                Context.Log(4.2);
                Context.Log(true);

                var Obj = new Dictionary<string, string>();
                Obj.Add("objectKey", "objectValue");

                Context.Log(Obj);

                var Arr = new List<string>();
                Arr.Add("arrayValue");

                Context.Log(Arr);

                return Context.Res.Send("");
            case "library":
                var response = await http.GetStringAsync($"https://jsonplaceholder.typicode.com/todos/{Context.Req.BodyString}");
                var todo = JsonSerializer.Deserialize<Dictionary<string, object>>(response) ?? new Dictionary<string, object>();

                return Context.Res.Json(new()
                {
                    { "todo", todo }
                });
            default:
                throw new Exception("Unkonwn action");
        }
    }
}
