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
    if (challengeId != Environment.GetEnvironmentVariable("INTERNAL_RUNTIME_KEY"))
    {
        return Results.Unauthorized();
    }

    try
    {
        var response = await new Wrapper().Main(
            req: request ?? new(),
            res: new());

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
