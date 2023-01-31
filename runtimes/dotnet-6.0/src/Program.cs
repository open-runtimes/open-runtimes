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
    if(Secret != Environment.GetEnvironmentVariable("OPEN_RUNTIMES_SECRET"))
    {
        return new CustomResponse("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.", 500);
    }

    StreamReader Reader = new StreamReader(Request.Body);
    string RawBody = await Reader.ReadToEndAsync();
    object Body = RawBody;
    Dictionary<string, string> Headers = new Dictionary<string, string>();
    string Method = Request.Method;
    string Url = Request.Path + Request.QueryString;

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
        Body = JsonConvert.DeserializeObject<Dictionary<string, object>>(RawBody);
    }

    RuntimeRequest ContextRequest = new RuntimeRequest(RawBody, Body, Headers, Method, Url);
    RuntimeResponse ContextResponse = new RuntimeResponse();
    RuntimeContext Context = new RuntimeContext(ContextRequest, ContextResponse);

    var Customstd = new StringBuilder();
    var CustomstdWriter = new StringWriter(Customstd);
    Console.SetOut(CustomstdWriter);
    Console.SetError(CustomstdWriter);

    RuntimeOutput? Output = null;

    try
    {
        var CodeWrapper = new Wrapper();
        Output = await CodeWrapper.Main(Context);
    }
    catch (Exception e)
    {
        Context.Error(e.ToString());
        Output = Context.Res.Send("", 500, new Dictionary<string,string>());
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

    if(!String.IsNullOrEmpty(Customstd.ToString())) {
        Context.Log("Unsupported log noticed. Use Context.Log() or Context.Error() for logging.");
    }

    OutputHeaders.Add("x-open-runtimes-logs", System.Web.HttpUtility.UrlEncode(String.Join("\n", Context._Logs.Cast<string>().ToArray())));
    OutputHeaders.Add("x-open-runtimes-errors", System.Web.HttpUtility.UrlEncode(String.Join("\n", Context._Errors.Cast<string>().ToArray())));

    return new CustomResponse(Output.Body, Output.StatusCode, OutputHeaders);
}
