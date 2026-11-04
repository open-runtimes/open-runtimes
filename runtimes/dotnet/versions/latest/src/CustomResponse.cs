using System.Text;

namespace DotNetRuntime
{
    class CustomResponse : IResult
    {
        private readonly byte[] _body;
        private readonly int _statusCode;
        private readonly Dictionary<string, string> _headers;
        private readonly RuntimeLogger? _logger;

        public CustomResponse(
            byte[] body,
            int statusCode,
            Dictionary<string, string>? headers = null,
            RuntimeLogger? logger = null
        )
        {
            _body = body;
            _statusCode = statusCode;
            _headers = headers ?? new Dictionary<string, string>();
            _logger = logger;
        }

        // With OPEN_RUNTIMES_LOGGING=inline, the body goes out in a `b` frame and the logs
        // and errors follow in `l` and `e` frames. Each frame is its type, the payload's
        // byte length in hex and a newline, then the payload.
        private static void WriteFrame(MemoryStream framed, string type, byte[] payload)
        {
            if (payload.Length == 0)
            {
                return;
            }

            framed.Write(Encoding.UTF8.GetBytes(type + payload.Length.ToString("x") + "\n"));
            framed.Write(payload);
        }

        public Task ExecuteAsync(HttpContext httpContext)
        {
            string contentTypeValue = "text/plain";

            if (_headers.TryGetValue("content-type", out string contentType))
            {
                contentTypeValue = contentType;
            }

            if (
                !contentTypeValue.StartsWith("multipart/") && !contentTypeValue.Contains("charset=")
            )
            {
                contentTypeValue = contentTypeValue + "; charset=utf-8";
            }

            foreach (var entry in _headers)
            {
                httpContext.Response.Headers.Add(entry.Key, entry.Value);
            }

            var body = _body;
            var statusCode = _statusCode;

            if (_logger != null && _logger.inline)
            {
                // Framing gives the body a length of its own and needs a status that allows
                // a body, so the real ones travel in headers
                httpContext.Response.Headers.Add("x-open-runtimes-logs-inline", "enabled");
                httpContext.Response.Headers.Add(
                    "x-open-runtimes-content-length",
                    body.Length.ToString()
                );
                if (statusCode == 204 || statusCode == 304)
                {
                    httpContext.Response.Headers.Add(
                        "x-open-runtimes-status-code",
                        statusCode.ToString()
                    );
                    statusCode = 200;
                }

                var framed = new MemoryStream();
                WriteFrame(framed, "b", body);
                WriteFrame(framed, "l", _logger.inlineLogs.ToArray());
                WriteFrame(framed, "e", _logger.inlineErrors.ToArray());
                body = framed.ToArray();
            }

            httpContext.Response.StatusCode = statusCode;
            httpContext.Response.ContentType = contentTypeValue;
            httpContext.Response.ContentLength = body.Length;

            return httpContext.Response.Body.WriteAsync(body, 0, body.Length);
        }
    }
}
