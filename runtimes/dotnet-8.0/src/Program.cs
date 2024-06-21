using System.Text;
using System.Text.Json;

namespace DotNetRuntime
{
    public class Program
    {
        public static void Main(string[] args)
        {
            var app = WebApplication.Create(args);
            app.Urls.Add("http://0.0.0.0:3000");
            app.MapMethods("/{*path}", ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS", "HEAD", "TRACE"], Execute);
            app.Run();
        }

        static async Task<IResult> Execute(HttpRequest request, HttpContext context)
        {
            var loggingHeader = request.Headers.TryGetValue("x-open-runtimes-logging", out var loggingHeaderValue)
                ? loggingHeaderValue.ToString()
                : string.Empty;

            var logIdHeader = request.Headers.TryGetValue("x-open-runtimes-log-id", out var logIdHeaderValue)
                ? logIdHeaderValue.ToString()
                : string.Empty;

            RuntimeLogger logger = new RuntimeLogger(loggingHeader, logIdHeader);

            try
            {
                return await Action(request, logger, context);
            } catch (Exception e)
            {
                logger.Write(e.ToString(), RuntimeLogger.TYPE_ERROR);
                logger.End();

                var outputHeaders = new Dictionary<string, string>();
                outputHeaders.Add("x-open-runtimes-log-id", logger.id);

                return new CustomResponse(Encoding.UTF8.GetBytes(""), 500, outputHeaders);
            }
        }

        static async Task<IResult> Action(HttpRequest request, RuntimeLogger logger, HttpContext httpContext)
        {
            int safeTimout = -1;
            var timeout = request.Headers.TryGetValue("x-open-runtimes-timeout", out var timeoutValue)
                ? timeoutValue.ToString()
                : string.Empty;

            if(!string.IsNullOrEmpty(timeout))
            {
                if(!Int32.TryParse(timeout, out safeTimout) || safeTimout == 0)
                {
                    return new CustomResponse("Header \"x-open-runtimes-timeout\" must be an integer greater than 0."u8.ToArray(), 500);
                }
            }

            var secret = request.Headers.TryGetValue("x-open-runtimes-secret", out var secretValue)
                ? secretValue.ToString()
                : string.Empty;

            string serverSecret = Environment.GetEnvironmentVariable("OPEN_RUNTIMES_SECRET");
            if (!string.IsNullOrEmpty(serverSecret) && secret != serverSecret)
            {
                return new CustomResponse("Unauthorized. Provide correct \"x-open-runtimes-secret\" header."u8.ToArray(), 500);
            }

            byte[] bodyBinary = [];
            System.IO.Stream bodyStream = request.Body;

            using (MemoryStream memoryStream = new MemoryStream())
            {
                bodyStream.CopyToAsync(memoryStream);
                bodyBinary = memoryStream.ToArray();
            }
            
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

            String enforcedHeadersString = Environment.GetEnvironmentVariable("OPEN_RUNTIMES_HEADERS");
            if (string.IsNullOrEmpty(enforcedHeadersString))
            {
                enforcedHeadersString = "{}";
            }

            Dictionary<string, object> enforcedHeaders = JsonSerializer.Deserialize<Dictionary<string, object>>(enforcedHeadersString) ?? new Dictionary<string, object>();
            foreach(KeyValuePair<string, object> entry in enforcedHeaders)
            {
                headers[entry.Key.ToLower()] = Convert.ToString(entry.Value);
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
                bodyBinary);

            var contextResponse = new RuntimeResponse(httpContext.Response, logger);

            var context = new RuntimeContext(contextRequest, contextResponse, logger);

            logger.OverrideNativeLogs();

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
                        if (context.Res.ChunkHeaderSent)
                        {
                            output = context.Res.End();
                        }else
                        {
                            output = context.Res.Text("", 500);
                        }
                    }
                } else
                {
                    output = await new Handler().Main(context);
                }
            }
            catch (Exception e)
            {
                context.Error(e.ToString());
                if (context.Res.ChunkHeaderSent)
                {
                    output = context.Res.End();
                }else
                {
                    output = context.Res.Text("", 500);
                }
            }
            finally
            {
                logger.RevertNativeLogs();
            }

            if(output == null)
            {
                context.Error("Return statement missing. return context.Res.Empty() if no response is expected.");
                if (context.Res.ChunkHeaderSent)
                {
                    output = context.Res.End();
                }else
                {
                    output = context.Res.Text("", 500);
                }
            }

            var outputHeaders = new Dictionary<string, string>();
            foreach (var entry in output.Headers)
            {
                var header = entry.Key.ToLower();
                var value = entry.Value;

                if (header == "content-type" && !string.IsNullOrEmpty(value))
                {
                    value = value.ToLower();
                }

                if (!(header.StartsWith("x-open-runtimes-")))
                {
                    outputHeaders.Add(header, value);
                }
            }

            logger.End();
            outputHeaders.TryAdd("x-open-runtimes-log-id", logger.id);

           if (context.Res.ChunkHeaderSent)
            {
                return new CustomBinaryResponse(outputHeaders);
            }
            return new CustomResponse(output.Body, output.StatusCode, outputHeaders);
        }
    }

}
