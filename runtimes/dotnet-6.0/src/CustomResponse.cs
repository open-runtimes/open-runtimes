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
            if(headers == null) {
                headers = new Dictionary<string,string>();
            }

            _body = body;
            _statusCode = statusCode;
            _headers = headers;
        }

        public Task ExecuteAsync(HttpContext httpContext)
        {
            string contentType = _headers.TryGetValue("content-type", out var contentTypeValue) ? (string) contentTypeValue : "plain/text";

            httpContext.Response.StatusCode = _statusCode;
            httpContext.Response.ContentType = contentType;
            httpContext.Response.ContentLength = Encoding.UTF8.GetByteCount(_body);
            return httpContext.Response.WriteAsync(_body);
        }
    }
}