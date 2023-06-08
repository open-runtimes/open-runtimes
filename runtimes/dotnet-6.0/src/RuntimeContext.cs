using System.Collections; 
using System.Text.Json;

namespace DotNetRuntime
{
	public class RuntimeContext
	{
		public RuntimeRequest Req { get; set; }
		public RuntimeResponse Res { get; set; }

		public List<string> Logs { get; private set; } = new List<string>();
		public List<string> Errors { get; private set; } = new List<string>();

		public RuntimeContext(RuntimeRequest req, RuntimeResponse res)
		{
			Req = req;
			Res = res;
		}

		public void Log(object message)
		{
			if (message is IList || message is IDictionary) {
				Logs.Add(JsonSerializer.Serialize(message));
			} else {
				Logs.Add(message.ToString() ?? "");
			}
		}

		public void Error(object message)
		{
			if (message is IList || message is IDictionary) {
				Errors.Add(JsonSerializer.Serialize(message));
			} else {
				Errors.Add(message.ToString() ?? "");
			}
		}
	}
}

