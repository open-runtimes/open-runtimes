using System.Collections.Generic; 

namespace DotNetRuntime
{
	public class RuntimeOutput
	{
		public object Body { get; set; }
		public int StatusCode { get; set; }
		public Dictionary<string, string> Headers { get; set; }

		public RuntimeOutput(object body, int statusCode, Dictionary<string, string> headers)
		{
			Body = body;
			StatusCode = statusCode;
			Headers = headers;
		}
	}
}

