using System.Text.Json;

namespace DotNetRuntime {
    public class Handler {
        static readonly HttpClient http = new HttpClient();

        public async Task<RuntimeOutput> Main(RuntimeContext context)
        {
            string id = "1";

            if (!(context.Req.Body is String))
            {
                object? idObject;
                Dictionary<string, object> body = (Dictionary<string, object>) context.Req.Body;
                body.TryGetValue("id", out idObject);

                if(idObject != null) {
                    id = ((JsonElement) idObject).ToString();
                }
            }

            var response = await http.GetStringAsync($"https://jsonplaceholder.typicode.com/todos/{id}");
            var todo = JsonSerializer.Deserialize<Dictionary<string, object>>(response) ?? new Dictionary<string, object>();

            return context.Res.Json(new Dictionary<string, object?>()
            {
                { "message", "Hello Open Runtimes 👋" },
                { "todo", todo }
            });
        }
    }
}
