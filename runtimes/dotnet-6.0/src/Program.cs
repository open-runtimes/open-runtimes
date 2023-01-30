using DotNetRuntime;
using Microsoft.AspNetCore.Mvc;
using System.IO;
using System.Text;
using System.Text.Json;
using Newtonsoft.Json;
using System.Web;

var app = WebApplication.Create(args);
app.Urls.Add("http://0.0.0.0:3000");
app.MapMethods("/{*path}", new[] { "GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS", "HEAD", "TRACE" }, Execute);
app.Run();

static async Task<IResult> Execute(HttpRequest request)
{
    string secret = request.Headers.TryGetValue("x-open-runtimes-secret", out var secretValue) ? (string) secretValue : "";
    if(secret != Environment.GetEnvironmentVariable("OPEN_RUNTIMES_SECRET")) {
        return new CustomResponse("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.", 500);
    }

    StreamReader reader = new StreamReader(request.Body);
    string rawBody = await reader.ReadToEndAsync();
    object body = rawBody;
    Dictionary<string, string> headers = new Dictionary<string, string>();
    string method = request.Method;
    string url = request.Path + request.QueryString;

    foreach (var entry in request.Headers) {
        var header = entry.Key;
        var value = entry.Value;
        header = header.ToLower();

        if(!(header.StartsWith("x-open-runtimes-"))) {
            headers.Add(header, value);
        }
    }

    string contentType = request.Headers.TryGetValue("content-type", out var contentTypeValue) ? (string) contentTypeValue : "";
    if(contentType.Contains("application/json")) {
        body = JsonConvert.DeserializeObject<Dictionary<string, object>>(rawBody);
    }

    RuntimeRequest contextRequest = new RuntimeRequest(rawBody, body, headers, method, url);
    RuntimeResponse contextResponse = new RuntimeResponse();
    RuntimeContext context = new RuntimeContext(contextRequest, contextResponse);

    var customstd = new StringBuilder();
    var customstdWriter = new StringWriter(customstd);
    Console.SetOut(customstdWriter);
    Console.SetError(customstdWriter);

    RuntimeOutput? output = null;

    try
    {
        var codeWrapper = new Wrapper();
        output = await codeWrapper.Main(context);
    }
    catch (Exception e)
    {
        context.error(e.ToString());
        output = context.res.send("", 500, new Dictionary<string,string>());
    }

    if(output == null) {
        context.error("Return statement missing. return context.res.empty() if no response is expected.");
        output = context.res.send("", 500, new Dictionary<string,string>());
    }

    var outputHeaders = new Dictionary<string, string>();

    foreach (var entry in output.headers) {
        var header = entry.Key;
        var value = entry.Value;
        header = header.ToLower();

        if(!(header.StartsWith("x-open-runtimes-"))) {
            outputHeaders.Add(header, value);
        }
    }

    if(String.IsNullOrEmpty(customstd.ToString())) {
        context.log("Unsupported log noticed. Use context.log() or context.error() for logging.");
    }

    outputHeaders.Add("x-open-runtimes-logs", System.Web.HttpUtility.UrlEncode(String.Join('\n', context._logs)));
    outputHeaders.Add("x-open-runtimes-errors", System.Web.HttpUtility.UrlEncode(String.Join('\n', context._errors)));

    return new CustomResponse(output.body, output.statusCode, outputHeaders);
}
