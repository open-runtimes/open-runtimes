namespace DotNetRuntime;

using System.Text.Json;

public class RuntimeResponse
{
    public RuntimeOutput Send(string Body, int StatusCode = 200, Dictionary<string, string>? Headers = null)
    {
        if(Headers == null)
        {
            Headers = new Dictionary<string,string>();
        }

        return new RuntimeOutput(Body, StatusCode, Headers);
    }

    public RuntimeOutput Json(Dictionary<string, object?> Json, int StatusCode = 200, Dictionary<string, string>? Headers = null)
    {
        if(Headers == null)
        {
            Headers = new Dictionary<string,string>();
        }

        Headers.Add("content-type", "application/json");
        return this.Send(JsonSerializer.Serialize(Json), StatusCode, Headers);
    }

    public RuntimeOutput Empty()
    {
        return this.Send("", 204, new Dictionary<string, string>());
    }

    public RuntimeOutput Redirect(String Url, int StatusCode = 301, Dictionary<string, string>? Headers = null)
    {
        if(Headers == null) {
            Headers = new Dictionary<string,string>();
        }

        Headers.Add("location", Url);
        return this.Send("", StatusCode, Headers);
    }
}