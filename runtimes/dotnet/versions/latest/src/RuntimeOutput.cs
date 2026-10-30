namespace DotNetRuntime
{
    public class RuntimeOutput
    {
        public byte[] Body { get; set; }
        public int StatusCode { get; set; }
        public Dictionary<string, string> Headers { get; set; }
        public IAsyncEnumerable<object>? Stream { get; set; }

        public RuntimeOutput(byte[] body, int statusCode, Dictionary<string, string> headers)
        {
            Body = body;
            StatusCode = statusCode;
            Headers = headers;
        }

        public RuntimeOutput(
            IAsyncEnumerable<object> stream,
            int statusCode,
            Dictionary<string, string> headers
        )
            : this(new byte[0], statusCode, headers)
        {
            Stream = stream;
        }
    }
}
