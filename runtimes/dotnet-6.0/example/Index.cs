namespace DotNetRuntime;

using Newtonsoft.Json;

public class Handler {
    static readonly HttpClient http = new();

    public async Task<RuntimeOutput> Main(RuntimeContext Context)
    {
        string id = "1";

        if (!(Context.Req.Body is String))
        {
            Dictionary<string, object> body = (Dictionary<string, object>) Context.Req.Body;
            id = body.TryGetValue("id", out var value) ? (string) value : "1";
        }

        var response = await http.GetStringAsync($"https://jsonplaceholder.typicode.com/todos/{id}");
        var todo = JsonConvert.DeserializeObject<Dictionary<string, object>>(response, settings: null);

        return Context.Res.Json(new()
        {
            { "message", "Hello Open Runtimes 👋" },
            { "todo", todo }
        });
    }
}
