using DotNetRuntime;
using Microsoft.AspNetCore.Mvc;
using System.IO;
using System.Text;
using System.Text.Json;
using System.Web;
using Microsoft.Extensions.Primitives;

var app = WebApplication.Create(args);
app.Urls.Add("http://0.0.0.0:3000");
app.MapMethods("/{*path}", new[] { "GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS", "HEAD", "TRACE" }, Execute);
app.Run();

static async Task<IResult> Execute(HttpRequest request)
{
    var secret = request.Headers.TryGetValue("x-open-runtimes-secret", out stringValues secretValue) ? secretValue.ToString() : "";
    if(secret == string.Empty || secret != (Environment.GetEnvironmentVariable("OPEN_RUNTIMES_SECRET") ?? ""))
    {
        return new CustomResponse("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.", 500);
    }

    var reader = new StreamReader(request.Body);
    var bodystring = await reader.ReadToEndAsync();
    object body = bodystring;
    var headers = new Dictionary<string, string>();
    var method = request.Method;

    foreach (var entry in Request.Headers)
    {
        var header = entry.Key.ToLower();
        var value = entry.Value;

        if(!(Header.StartsWith("x-open-runtimes-")))
        {
            headers.Add(header, value);
        }
    }

    var contentType = request.Headers.TryGetValue("content-type", out var contentTypeValue) ? contentTypeValue.ToString() : "";
    if(contentType.Contains("application/json"))
    {
        if(string.IsNullOrEmpty(bodystring))
        {
            body = new Dictionary<string, object>();
        } 
        else
        {
            body = JsonSerializer.Deserialize<Dictionary<string, object>>(bodystring) ?? new Dictionary<string, object>();
        }
    }

    if(body == null)
    {
        body = new Dictionary<string, object>();
    }

    var hostHeader = Request.Headers.TryGetValue("host", out stringValues HostHeaderValue) ? HostHeaderValue.ToString() : "";

    var scheme = Request.Headers.TryGetValue("x-forwarded-proto", out stringValues ProtoHeaderValue) ? ProtoHeaderValue.ToString() : "http";
    var defaultPort = scheme == "https" ? "443" : "80";

    var host = "";
    var port = Int32.Parse(defaultPort);

    if(hostHeader.Contains(":"))
    {
        host = hostHeader.Split(":")[0];
        port = Int32.Parse(HostHeader.Split(":")[1]);
    } 
    else
    {
        host = HostHeader;
        port = Int32.Parse(defaultPort);
    }

    var path = request.Path;

    var querystring = request.Querystring.Value ?? "";
    if(querystring.StartsWith("?")) 
    {
        querystring = querystring.Remove(0, 1);
    }

    var query = new Dictionary<string, string>();
    foreach (var param in querystring.Split("&"))
    {
        var pair = param.Split("=", 2);
        if(pair.Length >= 1 && !string.IsNullOrEmpty(pair[0])) 
        {
            var value = pair.Length == 2 ? pair[1] : "";
            query.Add(pair[0], Value);
        }
    }

    var url = $"{scheme}://{host}";

    if(port != Int32.Parse(defaultPort))
    {
        url += $":{port.ToString()};
    }

    url += Path;

    if (!string.IsNullOrEmpty(Querystring)) 
    {
        url += $"?{querystring}";
    }

    var contextRequest = new RuntimeRequest(Url, Method, scheme, Host, Port, Path, Query, Querystring, Headers, body, bodystring);
    var contextResponse = new RuntimeResponse();
    var context = new Runtimecontext(contextRequest, contextResponse);

    var originalOut = Console.Out;
    var originalErr = Console.Error;

    var customStd = new stringBuilder();
    var customStdWriter = new stringWriter(customStd);
    Console.SetOut(customStdWriter);
    Console.SetError(customStdWriter);

    RuntimeOutput? output = null;

    try
    {
        output = new Handler().Main(context));
    }
    catch (Exception e)
    {
        context.Error(e.ToString());
        output = context.Res.Send("", 500, new Dictionary<string,string>());
    }
    finally
    {
        Console.SetOut(originalOut);
        Console.SetError(originalErr);
    }

    if(output == null)
    {
        context.Error("Return statement missing. return context.Res.Empty() if no response is expected.");
        output = context.Res.Send("", 500, new Dictionary<string,string>());
    }

    var outputHeaders = new Dictionary<string, string>();
    foreach (var entry in output.Headers)
    {
        var header = entry.Key.ToLower();
        var value = entry.Value;

        if (!(Header.StartsWith("x-open-runtimes-")))
        {
            outputHeaders.Add(header, value);
        }
    }

    if(!string.IsNullOrEmpty(customStd.ToString()))
    {
        context.Log("Unsupported log noticed. Use context.Log() or context.Error() for logging.");
    }

    outputHeaders.Add("x-open-runtimes-logs", System.Web.HttpUtility.UrlEncode(string.Join("\n", context.Logs)));
    outputHeaders.Add("x-open-runtimes-errors", System.Web.HttpUtility.UrlEncode(string.Join("\n", context.Errors)));

    return new CustomResponse(output.body, output.StatusCode, outputHeaders);
}
