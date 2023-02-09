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

static async Task<IResult> Execute(HttpRequest Request)
{
    string Secret = Request.Headers.TryGetValue("x-open-runtimes-secret", out var SecretValue) ? (string) SecretValue : "";
    if(Secret == "" || Secret != (Environment.GetEnvironmentVariable("OPEN_RUNTIMES_SECRET") ?? ""))
    {
        return new CustomResponse("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.", 500);
    }

    StreamReader Reader = new StreamReader(Request.Body);
    string BodyString = await Reader.ReadToEndAsync();
    object Body = BodyString;
    Dictionary<string, string> Headers = new Dictionary<string, string>();
    string Method = Request.Method;

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

    string ContentType = Request.Headers.TryGetValue("content-type", out var ContentTypeValue) ? (string) ContentTypeValue : "";
    if(ContentType.Contains("application/json"))
    {
        if(String.IsNullOrEmpty(BodyString))
        {
            Body = new Dictionary<string, object>();
        } else
        {
            Body = JsonConvert.DeserializeObject<Dictionary<string, object>>(BodyString);
        }
    }

    if(Body == null)
    {
        Body = new Dictionary<string, object>();
    }

    String HostHeader = Request.Headers.TryGetValue("host", out var HostHeaderValue) ? (string) HostHeaderValue : "";

    String Scheme = Request.Headers.TryGetValue("x-forwarded-proto", out var ProtoHeaderValue) ? (string) ProtoHeaderValue : "http";
    String DefaultPort = Scheme == "https" ? "443" : "80";

    String Host = "";
    int Port = Int32.Parse(DefaultPort);

    if(HostHeader.Contains(":"))
    {
        Host = HostHeader.Split(":")[0];
        Port = Int32.Parse(HostHeader.Split(":")[1]);
    } else
    {
        Host = HostHeader;
        Port = Int32.Parse(DefaultPort);
    }

    String Path = Request.Path;

    String QueryString = Request.QueryString.Value ?? "";
    if(QueryString.StartsWith("?")) {
        QueryString = QueryString.Remove(0,1);
    }

    Dictionary<string, string> Query = new Dictionary<string, string>();

    foreach (String param in QueryString.Split("&"))
    {
        String[] pair = param.Split("=", 2);

        if(pair.Length >= 1 && !String.IsNullOrEmpty(pair[0])) {
            String Value = pair.Length == 2 ? pair[1] : "";
            Query.Add(pair[0], Value);
        }
    }

    String Url = Scheme + "://" + Host;

    if(Port != Int32.Parse(DefaultPort))
    {
        Url += ":" + Port.ToString();
    }

    Url += Path;

    if(!String.IsNullOrEmpty(QueryString)) {
        Url += "?" + QueryString;
    }

    RuntimeRequest ContextRequest = new RuntimeRequest(BodyString, Body, Headers, Method, Url, Host, Scheme, Path, QueryString, Query, Port);
    RuntimeResponse ContextResponse = new RuntimeResponse();
    RuntimeContext Context = new RuntimeContext(ContextRequest, ContextResponse);

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
