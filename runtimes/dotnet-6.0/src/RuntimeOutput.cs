namespace DotNetRuntime
{
	public class RuntimeOutput
	{
		public string Body { get; set; }
		public int StatusCode { get; set; }
		public Dictionary<string, string> Headers { get; set; }

		public RuntimeOutput(string Body, int StatusCode, Dictionary<string, string> Headers)
		{
			this.Body = Body;
			this.StatusCode = StatusCode;
			this.Headers = Headers;
		}
	}
}

