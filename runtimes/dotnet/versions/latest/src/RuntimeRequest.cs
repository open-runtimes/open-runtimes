using System.Text.Json;
using System.Text.Json.Serialization;

namespace DotNetRuntime
{
    public class RuntimeRequest
    {
        public string Method { get; private set; }
        public string Scheme { get; private set; }
        public string Host { get; private set; }
        public int Port { get; private set; }
        public string Path { get; private set; }
        public Dictionary<string, string> Query { get; private set; }
        public string QueryString { get; private set; }
        public string Url { get; private set; }
        public Dictionary<string, string> Headers { get; private set; }

        public object Body
        {
            get
            {
                var contentType = Headers.TryGetValue("content-type", out string? value)
                    ? (value ?? "").ToLower()
                    : "";

                if (contentType.StartsWith("application/json"))
                {
                    if (BodyBinary.Length == 0)
                    {
                        return new Dictionary<string, object?>();
                    }
                    else
                    {
                        return BodyJson;
                    }
                }

                return BodyText;
            }
        }
        public byte[] BodyBinary { get; private set; }
        public string BodyText =>
            System.Text.Encoding.UTF8.GetString(BodyBinary, 0, BodyBinary.Length);
        public string BodyRaw => BodyText;
        public Dictionary<string, object?> BodyJson =>
            JsonSerializer.Deserialize<Dictionary<string, object?>>(BodyText)
            ?? new Dictionary<string, object?>();

        public RuntimeRequest(
            string method,
            string scheme,
            string host,
            int port,
            string path,
            Dictionary<string, string> query,
            string queryString,
            string url,
            Dictionary<string, string> headers,
            byte[] bodyBinary
        )
        {
            BodyBinary = bodyBinary;
            Headers = headers;
            Method = method;
            Url = url;
            Host = host;
            Scheme = scheme;
            Path = path;
            QueryString = queryString;
            Query = query;
            Port = port;
        }
    }
}
