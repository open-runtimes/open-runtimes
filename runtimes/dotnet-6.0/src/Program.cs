using DotNetRuntime;
using Microsoft.AspNetCore.Mvc;
using System.IO;
using System.Text;
using System.Text.Json;

var app = WebApplication.Create(args);
app.Urls.Add("http://0.0.0.0:3000");
app.MapPost("/", Execute);
app.Run();

static async Task<IResult> Execute(
    [FromHeader(Name = "x-internal-challenge")] string? challengeId,
    [FromBody] RuntimeRequest? request)
{
    if (challengeId is not null 
        && challengeId != Environment.GetEnvironmentVariable("INTERNAL_RUNTIME_KEY"))
    {
        return Results.Problem(
            detail: "Unauthorized",
            statusCode: 500);
    }

    var originalOut = Console.Out;
    var originalErr = Console.Error;
    var outString = new StringBuilder();
    var errString = new StringBuilder();

    try
    {
        var codeWrapper = new Wrapper();
        var req = request ?? new(); 
        var res = new RuntimeResponse();

        var outWriter = new StringWriter(outString);
        var errWriter = new StringWriter(errString);

        Console.SetOut(outWriter);
        Console.SetError(errWriter);

        var response = await codeWrapper.Main(req, res);
        var output = new Dictionary<string, object?>()
        {
            { "response", response.Data },
            { "stdout", outString.ToString() }
        };

        return Results.Text(
            content: JsonSerializer.Serialize(output),
            contentType: "application/json",
            contentEncoding: System.Text.Encoding.UTF8);
    }
    catch (Exception e)
    {
        Console.Error.Write(e);
        var output = new Dictionary<string, object?>()
        {
            { "stderr", errString.ToString() },
            { "stdout", outString.ToString() }
        };
        return Results.Text(
            content: JsonSerializer.Serialize(output),
            contentType: "application/json",
            contentEncoding: System.Text.Encoding.UTF8);
    }
    finally
    {
        Console.SetOut(originalOut);
        Console.SetError(originalErr);
    }
}
