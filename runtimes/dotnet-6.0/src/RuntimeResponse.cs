using System.Text.Json;

namespace DotNetRuntime;

public class RuntimeResponse
{
    public string Data { get; set; }
    public int StatusCode { get; set; }

    public RuntimeResponse(
        string data = "",
        int statusCode = 200)
    {
        Data = data;
        StatusCode = statusCode;
    }

    public RuntimeResponse Send(
        string data,
        int statusCode = 200)
    {
        Data = data;
        StatusCode = statusCode;
        return this;
    }

    public RuntimeResponse Json(
        Dictionary<string, object?> data,
        int statusCode = 200)
    {
        Data = JsonSerializer.Serialize(data);
        StatusCode = statusCode;
        return this;
    }
}


