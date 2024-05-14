using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using System.Web;
using System.Collections.Generic; 
using System.Text;

namespace DotNetRuntime
{
    class CustomResponse : IResult
    {
        private readonly object _body;
        private readonly int _statusCode;
        private readonly Dictionary<string, string> _headers;

        public CustomResponse(object body, int statusCode, Dictionary<string, string>? headers = null)
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

            if(_body is String) {
                httpContext.Response.ContentLength = Encoding.UTF8.GetByteCount(_body as String);
                return httpContext.Response.WriteAsync(_body as String);
            } else if(_body is byte[]) {
                httpContext.Response.ContentLength = (_body as byte[]).Length;
                return httpContext.Response.Body.WriteAsync(_body as byte[], 0, (_body as byte[]).Length);
            } else {
                httpContext.Response.ContentLength = Encoding.UTF8.GetByteCount("");
                return httpContext.Response.WriteAsync("");
            }
        }
    }
}