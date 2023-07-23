using System.Collections.Generic; 

namespace DotNetRuntime
{
	public class RuntimeOutput
	{
		public string Body { get; set; }
		public int StatusCode { get; set; }
		public Dictionary<string, string> Headers { get; set; }

		public RuntimeOutput(string body, int statusCode, Dictionary<string, string> headers)
		{
			Body = body;
			StatusCode = statusCode;
			Headers = headers;
		}
	}
}

