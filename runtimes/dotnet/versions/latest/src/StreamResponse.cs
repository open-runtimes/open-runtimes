using System.Text;

namespace DotNetRuntime
{
    class StreamResponse : IResult
    {
        private readonly IAsyncEnumerable<object> _source;
        private readonly int _statusCode;
        private readonly Dictionary<string, string> _headers;
        private readonly RuntimeContext _context;
        private readonly RuntimeLogger _logger;
        private readonly int _timeout;

        public StreamResponse(
            IAsyncEnumerable<object> source,
            int statusCode,
            Dictionary<string, string> headers,
            RuntimeContext context,
            RuntimeLogger logger,
            int timeout
        )
        {
            _source = source;
            _statusCode = statusCode;
            _headers = headers;
            _context = context;
            _logger = logger;
            _timeout = timeout;
        }

        public async Task ExecuteAsync(HttpContext httpContext)
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
                // A streamed response is chunked, so its length is unknown.
                if (entry.Key != "content-length")
                {
                    httpContext.Response.Headers.Add(entry.Key, entry.Value);
                }
            }

            // Status and headers leave with the first chunk, so they are final from here on.
            httpContext.Response.StatusCode = _statusCode;
            httpContext.Response.ContentType = contentTypeValue;

            string? failure = null;
            var deadline = DateTime.UtcNow.AddSeconds(_timeout);
            var enumerator = _source.GetAsyncEnumerator();

            _logger.OverrideNativeLogs();
            try
            {
                while (true)
                {
                    var next = enumerator.MoveNextAsync().AsTask();

                    if (_timeout > 0)
                    {
                        var remaining = deadline - DateTime.UtcNow;
                        if (
                            remaining <= TimeSpan.Zero
                            || await Task.WhenAny(next, Task.Delay(remaining)) != next
                        )
                        {
                            failure = "Execution timed out.";
                            break;
                        }
                    }

                    if (!await next)
                    {
                        break;
                    }

                    var bytes = enumerator.Current is byte[] binary
                        ? binary
                        : Encoding.UTF8.GetBytes(enumerator.Current?.ToString() ?? "");

                    if (bytes.Length > 0)
                    {
                        await httpContext.Response.Body.WriteAsync(bytes, 0, bytes.Length);
                        await httpContext.Response.Body.FlushAsync();
                    }
                }
            }
            catch (Exception e)
            {
                failure = e.ToString();
            }
            finally
            {
                _logger.RevertNativeLogs();
            }

            if (failure != null)
            {
                _context.Error(failure);
            }

            // Logs are read once the response finishes, so they are flushed first.
            _logger.End();

            // Content is already on the wire, so a failure can only reach the caller as a truncation.
            if (failure != null)
            {
                httpContext.Abort();
            }
        }
    }
}
