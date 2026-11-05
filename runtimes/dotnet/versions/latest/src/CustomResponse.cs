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
            httpContext.Response.ContentLength = _body.Length;

            return httpContext.Response.Body.WriteAsync(_body, 0, _body.Length);
        }
    }
}
