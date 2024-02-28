using System.Text;

namespace DotNetRuntime
{
    class CustomResponse : IResult
    {
        private readonly string _body;
        private readonly int _statusCode;
        private readonly Dictionary<string, string> _headers;

        public CustomResponse(string body, int statusCode, Dictionary<string, string>? headers = null)
        {
            _body = body;
            _statusCode = statusCode;
            _headers = headers ?? new Dictionary<string, string>();
        }

        public Task ExecuteAsync(HttpContext httpContext)
        {
            string contentTypeValue = "text/plain";
            
            if (_headers.TryGetValue("content-type", out string contentType)) {
                 contentTypeValue = contentType;
            } 

            if (!contentTypeValue.StartsWith("multipart/") &&
                !contentTypeValue.Contains("charset="))
            {
                contentTypeValue = contentTypeValue + "; charset=utf-8";
            }

            foreach (var entry in _headers)
            {
                httpContext.Response.Headers.Add(entry.Key, entry.Value);
            }

            httpContext.Response.StatusCode = _statusCode;
            httpContext.Response.ContentType = contentTypeValue;
            httpContext.Response.ContentLength = Encoding.UTF8.GetByteCount(_body);

            return httpContext.Response.WriteAsync(_body);
        }
    }
}