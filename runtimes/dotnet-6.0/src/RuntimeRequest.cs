namespace DotNetRuntime
{
	public class RuntimeRequest
	{
		public string RawBody { get; set; }
		public object Body { get; set; }
		public Dictionary<string, string> Headers { get; set; }
		public string Method { get; set; }
		public string Url { get; set; }

		public RuntimeRequest(string RawBody, object Body, Dictionary<string, string> Headers, string Method, string Url)
		{
			this.RawBody = RawBody;
			this.Body = Body;
			this.Headers = Headers;
			this.Method = Method;
			this.Url = Url;
		}
	}
}

