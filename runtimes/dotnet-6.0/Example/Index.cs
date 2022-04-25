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

    var response = await http.GetStringAsync($"https://jsonplaceholder.typicode.com/todos/{id}");
    var todo = JsonConvert.DeserializeObject<Dictionary<string, object>>(response, settings: null);

    return res.Json(new()
    {
        { "message", "Hello Open Runtimes 👋" },
        { "todo", todo }
    });
}