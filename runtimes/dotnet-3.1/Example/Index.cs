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

    var response = await http.GetStringAsync($"https://jsonplaceholder.typicode.com/todos/{id}");
    var todo = JsonConvert.DeserializeObject<Dictionary<string, object>>(response, settings: null);

    return res.Json(new Dictionary<string, object>()
    {
        { "message", "Hello Open Runtimes 👋" },
        { "todo", todo }
    });
}