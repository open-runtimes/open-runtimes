using Newtonsoft.Json;

static readonly HttpClient http = new();

public async Task<RuntimeOutput> Main(RuntimeContext context)
{
    string id = "1";

    if (!(context.req.body is String))
    {
        Dictionary<string, object> body = (Dictionary<string, object>) context.req.body;
        id = body.TryGetValue("id", out var value) ? (string) value : "1";
    }

    var response = await http.GetStringAsync($"https://jsonplaceholder.typicode.com/todos/{id}");
    var todo = JsonConvert.DeserializeObject<Dictionary<string, object>>(response, settings: null);

    return context.res.json(new()
    {
        { "message", "Hello Open Runtimes 👋" },
        { "todo", todo }
    });
}