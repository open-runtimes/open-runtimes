namespace DotNetRuntime
{
	public class RuntimeOutput
	{
		public string body { get; set; }
		public int statusCode { get; set; }
		public Dictionary<string, string> headers { get; set; }

		public RuntimeOutput(string body, int statusCode, Dictionary<string, string> headers)
		{
			this.body = body;
			this.statusCode = statusCode;
			this.headers = headers;
		}
	}
}

