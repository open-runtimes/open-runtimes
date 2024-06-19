namespace DotNetRuntime
{
    public class RuntimeOutput
    {
        public byte[] Body { get; set; }
        public int StatusCode { get; set; }
        public Dictionary<string, string> Headers { get; set; }

        public bool Chunked { get; set; }
        public RuntimeOutput(byte[] body, int statusCode, Dictionary<string, string> headers, bool chunked)
        {
            Body = body;
            StatusCode = statusCode;
            Headers = headers;
            Chunked = chunked;
        }
    }
}