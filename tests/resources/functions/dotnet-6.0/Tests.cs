namespace DotNetRuntime;

using System;
using Newtonsoft.Json;
using System.Collections;

public class Handler {
    static readonly HttpClient http = new();

    public async Task<RuntimeOutput> Main(RuntimeContext Context)
    {
        string Action = Context.Req.Headers.TryGetValue("x-action", out var ActionValue) ? (string) ActionValue : "";

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
                Dictionary<string, string> Headers = new Dictionary<string, string>();
                Headers.Add("first-header", "first-value");
                string SecondHeader = Context.Req.Headers.TryGetValue("x-open-runtimes-custom-in-header", out var SecondHeaderValue) ? (string) SecondHeaderValue : "missing";
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
                Dictionary<string, object> Json = new Dictionary<string, object>();

                foreach (var Entry in Context.Req.Headers)
                {
                    Json.Add(Entry.Key, Entry.Value);
                }

                return Context.Res.Json(Json);
            case "requestBodyPlaintext":
                return Context.Res.Send((string) Context.Req.Body);
            case "requestBodyJson":
                string Key1 = "";
                string Key2 = "";

                if(Context.Req.Body is string) {
                    Key1 = "Missing key";
                    Key2 = "Missing key";
                } else {
                    Dictionary<String, Object> Body = (Dictionary<String, Object>) Context.Req.Body;

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

                Dictionary<string, string> Obj = new Dictionary<string, string>();
                Obj.Add("objectKey", "objectValue");

                Context.Log(Obj);

                ArrayList Arr = new ArrayList();
                Arr.Add("arrayValue");

                Context.Log(Arr);

                return Context.Res.Send("");
            case "library":
                var response = await http.GetStringAsync($"https://jsonplaceholder.typicode.com/todos/" + Context.Req.BodyString);
                var todo = JsonConvert.DeserializeObject<Dictionary<string, object>>(response, settings: null);

                return Context.Res.Json(new()
                {
                    { "todo", todo }
                });
            default:
                throw new Exception("Unkonwn action");
        }
    }
}
