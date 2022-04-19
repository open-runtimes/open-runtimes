using DotNetRuntime;
using Microsoft.AspNetCore.Mvc;

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

    try
    {
        var codeWrapper = new Wrapper();
        var req = request ?? new(); 
        var res = new RuntimeResponse();
        var response = await codeWrapper.Main(req, res);

        return Results.Text(
            content: response.Data,
            contentType: "application/json",
            contentEncoding: System.Text.Encoding.UTF8);
    }
    catch (Exception e)
    {
        return Results.Problem(
            detail: e.Message,
            statusCode: 500);
    }
}
