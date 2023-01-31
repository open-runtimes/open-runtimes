using System.Text;

namespace DotNetRuntime
{
    class CustomResponse : IResult
    {
        private readonly string _Body;
        private readonly int _StatusCode;
        private readonly Dictionary<string, string> _Headers;

        public CustomResponse(string Body, int StatusCode, Dictionary<string, string>? Headers = null)
        {
            if(Headers == null)
            {
                Headers = new Dictionary<string,string>();
            }

            _Body = Body;
            _StatusCode = StatusCode;
            _Headers = Headers;
        }

        public Task ExecuteAsync(HttpContext HttpContext)
        {
            string contentType = _Headers.TryGetValue("content-type", out var contentTypeValue) ? (string) contentTypeValue : "plain/text";

            foreach (var Entry in _Headers)
            {
                HttpContext.Response.Headers.Add(Entry.Key, Entry.Value);
            }
            HttpContext.Response.StatusCode = _StatusCode;
            HttpContext.Response.ContentType = contentType;
            HttpContext.Response.ContentLength = Encoding.UTF8.GetByteCount(_Body);
            return HttpContext.Response.WriteAsync(_Body);
        }
    }
}