using System.Text;

namespace DotNetRuntime
{
    class CustomResponse : IResult
    {
        private readonly byte[] _body;
        private readonly int _statusCode;
        private readonly Dictionary<string, string> _headers;
        private readonly IAsyncEnumerable<object>? _stream;
        private readonly RuntimeContext? _context;
        private readonly DateTime? _deadline;

        public CustomResponse(
            byte[] body,
            int statusCode,
            Dictionary<string, string>? headers = null
        )
        {
            _body = body;
            _statusCode = statusCode;
            _headers = headers ?? new Dictionary<string, string>();
        }

        public CustomResponse(
            IAsyncEnumerable<object> stream,
            int statusCode,
            Dictionary<string, string> headers,
            RuntimeContext context,
            DateTime? deadline
        )
            : this(new byte[0], statusCode, headers)
        {
            _stream = stream;
            _context = context;
            _deadline = deadline;
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
                httpContext.Response.Headers.Add(entry.Key, entry.Value);
            }

            httpContext.Response.StatusCode = _statusCode;
            httpContext.Response.ContentType = contentTypeValue;

            if (_stream == null)
            {
                httpContext.Response.ContentLength = _body.Length;
                await httpContext.Response.Body.WriteAsync(_body, 0, _body.Length);
                return;
            }

            httpContext.Response.ContentLength = null;
            await httpContext.Response.Body.FlushAsync();

            using var stop = CancellationTokenSource.CreateLinkedTokenSource(
                httpContext.RequestAborted
            );
            if (_deadline != null)
            {
                stop.CancelAfter(
                    Math.Max(0, (int)(_deadline.Value - DateTime.UtcNow).TotalMilliseconds)
                );
            }

            var chunks = _stream.GetAsyncEnumerator(stop.Token);
            var next = chunks.MoveNextAsync().AsTask();
            var failed = false;

            try
            {
                while (await next.WaitAsync(stop.Token))
                {
                    var bytes = chunks.Current is byte[] binary
                        ? binary
                        : Encoding.UTF8.GetBytes(chunks.Current?.ToString() ?? "");
                    await httpContext.Response.Body.WriteAsync(bytes, stop.Token);
                    next = chunks.MoveNextAsync().AsTask();
                }
            }
            catch (OperationCanceledException) when (stop.IsCancellationRequested)
            {
                failed = true;
                if (!httpContext.RequestAborted.IsCancellationRequested)
                {
                    _context!.Error("Execution timed out.");
                }
            }
            catch (Exception e)
            {
                failed = true;
                _context!.Error(e.ToString());
            }
            finally
            {
                // An async iterator throws if disposed while its MoveNextAsync is pending
                _ = next.ContinueWith(_ => chunks.DisposeAsync().AsTask());
            }

            _context!.Logger.End();

            if (failed)
            {
                httpContext.Abort();
            }
        }
    }
}
