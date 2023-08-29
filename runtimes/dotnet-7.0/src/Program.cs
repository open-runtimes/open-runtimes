using DotNetRuntime;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Mvc;
using System.IO;
using System.Text;
using System.Text.Json;
using System.Web;
using Microsoft.Extensions.Primitives;

namespace DotNetRuntime
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var app = WebApplication.Create(args);
            app.Urls.Add("http://0.0.0.0:3000");
            app.MapMethods("/{*path}", new[] { "GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS", "HEAD", "TRACE" }, Execute);
            app.Run();
        }

        static async Task<IResult> Execute(HttpRequest request)
        {
            int safeTimout = -1;
            var timeout = request.Headers.TryGetValue("x-open-runtimes-timeout", out var timeoutValue)
                ? timeoutValue.ToString()
                : string.Empty;

            if(!string.IsNullOrEmpty(timeout))
            {
                if(!Int32.TryParse(timeout, out safeTimout) || safeTimout == 0)
                {
                    return new CustomResponse("Header \"x-open-runtimes-timeout\" must be an integer greater than 0.", 500);
                }
            }

            var secret = request.Headers.TryGetValue("x-open-runtimes-secret", out var secretValue)
                ? secretValue.ToString()
                : string.Empty;

            if(secret == string.Empty || secret != (Environment.GetEnvironmentVariable("OPEN_RUNTIMES_SECRET") ?? ""))
            {
                return new CustomResponse("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.", 500);
            }

            var reader = new StreamReader(request.Body);
            var bodyRaw = await reader.ReadToEndAsync();
            object body = bodyRaw;
            var headers = new Dictionary<string, string>();
            var method = request.Method;

            foreach (var entry in request.Headers)
            {
                var header = entry.Key.ToLower();
                var value = entry.Value;

                if(!header.StartsWith("x-open-runtimes-"))
                {
                    headers.Add(header, value);
                }
            }

            var contentType = request.Headers.TryGetValue("content-type", out var contentTypeValue) ? contentTypeValue.ToString() : "";
            if(contentType.Contains("application/json"))
            {
                if(string.IsNullOrEmpty(bodyRaw))
                {
                    body = new Dictionary<string, object>();
                } 
                else
                {
                    body = JsonSerializer.Deserialize<Dictionary<string, object>>(bodyRaw) ?? new Dictionary<string, object>();
                }
            }

            if(body == null)
            {
                body = new Dictionary<string, object>();
            }

            var hostHeader = request.Headers.TryGetValue("host", out var hostHeaderValue)
                ? hostHeaderValue.ToString()
                : string.Empty;

            var scheme = request.Headers.TryGetValue("x-forwarded-proto", out var protoHeaderValue)
                ? protoHeaderValue.ToString()
                : "http";

            var defaultPort = scheme == "https"
                ? "443"
                : "80";

            var host = string.Empty;
            var port = Int32.Parse(defaultPort);

            if(hostHeader.Contains(":"))
            {
                host = hostHeader.Split(":")[0];
                port = Int32.Parse(hostHeader.Split(":")[1]);
            } 
            else
            {
                host = hostHeader;
                port = Int32.Parse(defaultPort);
            }

            var path = request.Path;

            var queryString = request.QueryString.Value ?? "";
            if(queryString.StartsWith("?")) 
            {
                queryString = queryString.Remove(0, 1);
            }

            var query = new Dictionary<string, string>();
            foreach (var param in queryString.Split("&"))
            {
                var pair = param.Split("=", 2);
                if(pair.Length >= 1 && !string.IsNullOrEmpty(pair[0])) 
                {
                    var value = pair.Length == 2 ? pair[1] : "";
                    query.Add(pair[0], value);
                }
            }

            var url = $"{scheme}://{host}";

            if(port != Int32.Parse(defaultPort))
            {
                url += $":{port.ToString()}";
            }

            url += path;

            if (!string.IsNullOrEmpty(queryString))
            {
                url += $"?{queryString}";
            }

            var contextRequest = new RuntimeRequest(
                method,
                scheme,
                host,
                port,
                path,
                query,
                queryString,
                url,
                headers,
                body,
                bodyRaw);

            var contextResponse = new RuntimeResponse();

            var context = new RuntimeContext(contextRequest, contextResponse);

            var originalOut = Console.Out;
            var originalErr = Console.Error;

            var customStd = new StringBuilder();
            var customStdWriter = new StringWriter(customStd);
            Console.SetOut(customStdWriter);
            Console.SetError(customStdWriter);

            RuntimeOutput? output = null;

            try
            {
                if (safeTimout != -1)
                {
                    var result = await await Task.WhenAny<RuntimeOutput?>(
                        Task.Run<RuntimeOutput?>(async () => {
                            await Task.Delay(safeTimout * 1000);
                            return null;
                        }),
                        new Handler().Main(context)
                    );

                    if (result != null)
                    {
                        output = result;
                    }
                    else
                    {
                        context.Error("Execution timed out.");
                        output = context.Res.Send("", 500);
                    }
                } else
                {
                    output = await new Handler().Main(context);
                }
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

                if (!(header.StartsWith("x-open-runtimes-")))
                {
                    outputHeaders.Add(header, value);
                }
            }

            if(!string.IsNullOrEmpty(customStd.ToString()))
            {
                context.Log("");
                context.Log("----------------------------------------------------------------------------");
                context.Log("Unsupported logs detected. Use context.Log() or context.Error() for logging.");
                context.Log("----------------------------------------------------------------------------");
                context.Log(customStd.ToString());
                context.Log("----------------------------------------------------------------------------");
            }

            outputHeaders.Add("x-open-runtimes-logs", System.Web.HttpUtility.UrlEncode(string.Join("\n", context.Logs)));
            outputHeaders.Add("x-open-runtimes-errors", System.Web.HttpUtility.UrlEncode(string.Join("\n", context.Errors)));

            return new CustomResponse(output.Body, output.StatusCode, outputHeaders);
        }
    }
}




