using System.Collections.Generic;
using System.Net.Http;
using System.Threading.Tasks;
using Newtonsoft.Json;

private static readonly HttpClient http = new HttpClient();

public async Task<RuntimeResponse> Main(RuntimeRequest req, RuntimeResponse res)
{
    object id = "1";
    if (!string.IsNullOrEmpty(req.Payload))
    {
        var payload = JsonConvert.DeserializeObject<Dictionary<string, object>>(req.Payload, settings: null);
        id = payload?.TryGetValue("id", out id) == true ? id : "1";
    }

    var header = req.Headers.ContainsKey("x-test-header")
        ? req.Headers["x-test-header"]
        : "";

    var env = req.Env.ContainsKey("test-env")
        ? req.Env["test-env"]
        : "";

    var response = await http.GetStringAsync($"https://jsonplaceholder.typicode.com/todos/{id.ToString()}");
    var todo = JsonConvert.DeserializeObject<Dictionary<string, object>>(response, settings: null);

    return res.Json(new Dictionary<string, object>()
    {
        { "isTest", true },
        { "message", "Hello Open Runtimes 👋" },
        { "header", header },
        { "env", env },
        { "todo", todo }
    });
}