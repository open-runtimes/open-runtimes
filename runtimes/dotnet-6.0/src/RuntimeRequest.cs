namespace DotNetRuntime
{
	public class RuntimeRequest
	{
		public string BodyString { get; set; }
		public object Body { get; set; }
		public Dictionary<string, string> Headers { get; set; }
		public string Method { get; set; }
		public string Url { get; set; }
		public string Host { get; set; }
		public string Scheme { get; set; }
		public string Path { get; set; }
		public string QueryString { get; set; }
		public Dictionary<string, string> Query { get; set; }
		public int Port { get; set; }

		public RuntimeRequest(string BodyString, object Body, Dictionary<string, string> Headers, string Method, string Url, string Host, string Scheme, string Path, string QueryString, Dictionary<string, string> Query, int Port)
		{
			this.BodyString = BodyString;
			this.Body = Body;
			this.Headers = Headers;
			this.Method = Method;
			this.Url = Url;
			this.Host = Host;
			this.Scheme = Scheme;
			this.Path = Path;
			this.QueryString = QueryString;
			this.Query = Query;
			this.Port = Port;
		}
	}
}

