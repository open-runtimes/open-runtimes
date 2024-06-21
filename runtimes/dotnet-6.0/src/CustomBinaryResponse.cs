using System.Text;

namespace DotNetRuntime
{
    class CustomBinaryResponse : IResult
    {
        private readonly Dictionary<string, string> _headers;

        public CustomBinaryResponse(Dictionary<string, string>? headers = null)
        {
            _headers = headers ?? new Dictionary<string, string>();
        }

        public Task ExecuteAsync(HttpContext httpContext)
        {
            if (!httpContext.Response.SupportsTrailers()) return httpContext.Response.Body.FlushAsync();
            
            foreach (var entry in _headers)
            {
                httpContext.Response.AppendTrailer(entry.Key, entry.Value);
            }

            return httpContext.Response.Body.FlushAsync();
        }
    }
}