using System.Collections.Generic; 

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
		public object Body { get; private set; }
		public string BodyRaw { get; private set; }

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
		    object body,
		    string bodyRaw)
		{
			BodyRaw = bodyRaw;
			Body = body;
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

