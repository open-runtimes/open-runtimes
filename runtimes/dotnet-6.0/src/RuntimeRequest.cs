namespace DotNetRuntime
{
	public class RuntimeRequest
	{
		public string rawBody { get; set; }
		public object body { get; set; }
		public Dictionary<string, string> headers { get; set; }
		public string method { get; set; }
		public string url { get; set; }

		public RuntimeRequest(string rawBody, object body, Dictionary<string, string> headers, string method, string url)
		{
			this.rawBody = rawBody;
			this.body = body;
			this.headers = headers;
			this.method = method;
			this.url = url;
		}
	}
}

