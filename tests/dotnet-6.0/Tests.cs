using System;
using Newtonsoft.Json;

static readonly HttpClient http = new();

public async Task<RuntimeResponse> Main(RuntimeRequest req, RuntimeResponse res)
{
    string id = "1";
    if (!string.IsNullOrEmpty(req.Payload))
    {
        var payload = JsonConvert.DeserializeObject<Dictionary<string, object>>(req.Payload, settings: null);
        id = payload?.TryGetValue("id", out var value) == true
            ? value.ToString()!
            : "1";
    }

    var header = req.Headers.ContainsKey("x-test-header")
        ? req.Headers["x-test-header"]
        : "";

    var env = req.Env.ContainsKey("test-env")
        ? req.Env["test-env"]
        : "";

    var response = await http.GetStringAsync($"https://jsonplaceholder.typicode.com/todos/{id}");
    var todo = JsonConvert.DeserializeObject<Dictionary<string, object>>(response, settings: null);

    Console.WriteLine("log1");
    Console.WriteLine("{hello: world}");
    Console.WriteLine("[hello, world]");

    return res.Json(new()
    {
        { "isTest", true },
        { "message", "Hello Open Runtimes 👋" },
        { "header", header },
        { "env", env },
        { "todo", todo }
    });
}