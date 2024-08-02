namespace DotNetRuntime
{
    public class RuntimeOutput
    {
        public byte[] Body { get; set; }
        public int StatusCode { get; set; }
        public Dictionary<string, string> Headers { get; set; }

        public RuntimeOutput(byte[] body, int statusCode, Dictionary<string, string> headers)
        {
            Body = body;
            StatusCode = statusCode;
            Headers = headers;
        }
    }
}
