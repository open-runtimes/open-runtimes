namespace DotNetRuntime;

using System.Text.Json;

public class RuntimeResponse
{
    public RuntimeOutput send(string body, int statusCode = 200, Dictionary<string, string>? headers = null)
    {
        if(headers == null) {
            headers = new Dictionary<string,string>();
        }

        return new RuntimeOutput(body, statusCode, headers);
    }

    public RuntimeOutput json(Dictionary<string, object?> json, int statusCode = 200, Dictionary<string, string>? headers = null)
    {
        if(headers == null) {
            headers = new Dictionary<string,string>();
        }

        headers.Add("content-type", "application/json");
        return this.send(JsonSerializer.Serialize(json), statusCode, headers);
    }

    public RuntimeOutput empty()
    {
        return this.send("", 204, new Dictionary<string, string>());
    }

    public RuntimeOutput redirect(String url, int statusCode = 200, Dictionary<string, string>? headers = null)
    {
        if(headers == null) {
            headers = new Dictionary<string,string>();
        }

        headers.Add("location", url);
        return this.send("", statusCode, headers);
    }
}