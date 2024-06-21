namespace DotNetRuntime;

using System.Text.Json;

public class RuntimeResponse
{
    private readonly RuntimeLogger logger;
    private readonly HttpResponse httpContextResponse;
    public bool ChunkHeaderSent { get; set; } = false;

    public RuntimeResponse(HttpResponse httpContextResponse, RuntimeLogger logger)
    {
        this.httpContextResponse = httpContextResponse;
        this.logger = logger;
    }


    public RuntimeOutput Binary(byte[] bytes,int statusCode = 200, Dictionary<string, string>? headers = null ){
        return new RuntimeOutput(
            bytes,
            statusCode,
            headers ?? new Dictionary<string,string>(),
            false);
    }



    public RuntimeOutput Send(string body, int statusCode = 200, Dictionary<string, string>? headers = null)
    {
        return Text(
            body,
            statusCode,
            headers ?? new Dictionary<string,string>());
    }

    public RuntimeOutput Text(string body, int statusCode = 200, Dictionary<string, string>? headers = null)
    {
        return Binary(
            System.Text.Encoding.UTF8.GetBytes(body),
            statusCode,
            headers ?? new Dictionary<string,string>());
    }

    public RuntimeOutput Json(Dictionary<string, object?> json, int statusCode = 200, Dictionary<string, string>? headers = null)
    {
        if(headers == null)
        {
            headers = new Dictionary<string,string>();
        }

        headers.Add("content-type", "application/json");
        return Text(JsonSerializer.Serialize(json), statusCode, headers);
    }

    public RuntimeOutput Empty()
    {
        return Text("", 204, new Dictionary<string, string>());
    }

    public RuntimeOutput Redirect(String url, int statusCode = 301, Dictionary<string, string>? headers = null)
    {
        if(headers == null) {
            headers = new Dictionary<string,string>();
        }

        headers.Add("location", url);

        return Text("", statusCode, headers);
    }

    public void Start(int statusCode = 200, Dictionary<string, string>? headers = null)
    {
        headers ??= new Dictionary<string, string>();

        if (!ChunkHeaderSent)
        {
            ChunkHeaderSent = true;
            headers.TryAdd("cache-control", "no-store");
            headers.TryAdd("content-type", "text/event-stream");
            headers.TryAdd("connection", "keep-alive");
            headers.TryAdd("x-open-runtimes-log-id", logger.id);

            foreach(KeyValuePair<string, string> entry in headers)
            {
                httpContextResponse.Headers.Append(entry.Key,entry.Value);
            }

            httpContextResponse.StatusCode = statusCode;
        }
        else
        {
            throw new Exception("You can only call res.Start() once");
        }
    }

    public void WriteJson(object body)
    {
        WriteText(JsonSerializer.Serialize(body));
    }

    public void WriteText(string body)
    {

        WriteBinary(System.Text.Encoding.UTF8.GetBytes(body));
    }

    public void WriteBinary(byte[] body)
    {
        if (!ChunkHeaderSent)
        {
            throw new Exception("You must call res.Start() to start a chunk response");
        }
        httpContextResponse.Body.WriteAsync(body,0,body.Length);
        httpContextResponse.Body.FlushAsync();
    }

    public RuntimeOutput End()
    {
        headers ??= new Dictionary<string, string>();
        if (!ChunkHeaderSent)
        {
            throw new Exception("You must call res.Start() to start a chunk response");
        }

        return new RuntimeOutput(System.Text.Encoding.UTF8.GetBytes(""), 200, null, true);
    }
}
