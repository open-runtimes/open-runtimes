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

static async Task<IResult> Execute(HttpRequest Request)
{
    var Secret = Request.Headers.TryGetValue("x-open-runtimes-secret", out StringValues SecretValue) ? SecretValue.ToString() : "";
    if(Secret == "" || Secret != (Environment.GetEnvironmentVariable("OPEN_RUNTIMES_SECRET") ?? ""))
    {
        return new CustomResponse("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.", 500);
    }

    var Reader = new StreamReader(Request.Body);
    var BodyString = await Reader.ReadToEndAsync();
    object Body = BodyString;
    var Headers = new Dictionary<string, string>();
    var Method = Request.Method;

    foreach (var Entry in Request.Headers)
    {
        var Header = Entry.Key;
        var Value = Entry.Value;
        Header = Header.ToLower();

        if(!(Header.StartsWith("x-open-runtimes-")))
        {
            Headers.Add(Header, Value);
        }
    }

    var ContentType = Request.Headers.TryGetValue("content-type", out StringValues ContentTypeValue) ? ContentTypeValue.ToString() : "";
    if(ContentType.Contains("application/json"))
    {
        if(String.IsNullOrEmpty(BodyString))
        {
            Body = new Dictionary<string, object>();
        } else
        {
            Body = JsonSerializer.Deserialize<Dictionary<string, object>>(BodyString) ?? new Dictionary<string, object>();
        }
    }

    if(Body == null)
    {
        Body = new Dictionary<string, object>();
    }

    var HostHeader = Request.Headers.TryGetValue("host", out StringValues HostHeaderValue) ? HostHeaderValue.ToString() : "";

    var Scheme = Request.Headers.TryGetValue("x-forwarded-proto", out StringValues ProtoHeaderValue) ? ProtoHeaderValue.ToString() : "http";
    var DefaultPort = Scheme == "https" ? "443" : "80";

    var Host = "";
    var Port = Int32.Parse(DefaultPort);

    if(HostHeader.Contains(":"))
    {
        Host = HostHeader.Split(":")[0];
        Port = Int32.Parse(HostHeader.Split(":")[1]);
    } else
    {
        Host = HostHeader;
        Port = Int32.Parse(DefaultPort);
    }

    var Path = Request.Path;

    var QueryString = Request.QueryString.Value ?? "";
    if(QueryString.StartsWith("?")) {
        QueryString = QueryString.Remove(0,1);
    }

    var Query = new Dictionary<string, string>();

    foreach (var param in QueryString.Split("&"))
    {
        var pair = param.Split("=", 2);

        if(pair.Length >= 1 && !String.IsNullOrEmpty(pair[0])) {
            var Value = pair.Length == 2 ? pair[1] : "";
            Query.Add(pair[0], Value);
        }
    }

    var Url = Scheme + "://" + Host;

    if(Port != Int32.Parse(DefaultPort))
    {
        Url += ":" + Port.ToString();
    }

    Url += Path;

    if(!String.IsNullOrEmpty(QueryString)) {
        Url += "?" + QueryString;
    }


    var ContextRequest = new RuntimeRequest(Url, Method, Scheme, Host, Port, Path, Query, QueryString, Headers, Body, BodyString);
    var ContextResponse = new RuntimeResponse();
    var Context = new RuntimeContext(ContextRequest, ContextResponse);

    var originalOut = Console.Out;
    var originalErr = Console.Error;

    var Customstd = new StringBuilder();
    var CustomstdWriter = new StringWriter(Customstd);
    Console.SetOut(CustomstdWriter);
    Console.SetError(CustomstdWriter);

    RuntimeOutput? Output = null;

    try
    {
        var CodeHandler = new Handler();
        Output = await CodeHandler.Main(Context);
    }
    catch (Exception e)
    {
        Context.Error(e.ToString());
        Output = Context.Res.Send("", 500, new Dictionary<string,string>());
    }
    finally {
        Console.SetOut(originalOut);
        Console.SetError(originalErr);
    }

    if(Output == null)
    {
        Context.Error("Return statement missing. return Context.Res.Empty() if no response is expected.");
        Output = Context.Res.Send("", 500, new Dictionary<string,string>());
    }

    var OutputHeaders = new Dictionary<string, string>();

    foreach (var Entry in Output.Headers)
    {
        var Header = Entry.Key;
        var Value = Entry.Value;
        Header = Header.ToLower();

        if(!(Header.StartsWith("x-open-runtimes-")))
        {
            OutputHeaders.Add(Header, Value);
        }
    }

    if(!String.IsNullOrEmpty(Customstd.ToString()))
    {
        Context.Log("Unsupported log noticed. Use Context.Log() or Context.Error() for logging.");
    }

    OutputHeaders.Add("x-open-runtimes-logs", System.Web.HttpUtility.UrlEncode(String.Join("\n", Context.Logs.Cast<string>().ToArray())));
    OutputHeaders.Add("x-open-runtimes-errors", System.Web.HttpUtility.UrlEncode(String.Join("\n", Context.Errors.Cast<string>().ToArray())));

    return new CustomResponse(Output.Body, Output.StatusCode, OutputHeaders);
}
