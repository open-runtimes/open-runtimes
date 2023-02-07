using System.Collections; 
using System.Text.Json;

namespace DotNetRuntime
{
	public class RuntimeContext
	{
		public RuntimeRequest Req { get; set; }
		public RuntimeResponse Res { get; set; }

		public ArrayList Logs = new ArrayList();
		public ArrayList Errors = new ArrayList();

		public RuntimeContext(RuntimeRequest Req, RuntimeResponse Res)
		{
			this.Req = Req;
			this.Res = Res;
		}

		public void Log(object Message)
		{
			if (Message is IList || Message is IDictionary) {
				this.Logs.Add(JsonSerializer.Serialize(Message));
			} else {
				this.Logs.Add(Message.ToString());
			}
		}

		public void Error(object Message)
		{
			if (Message is IList || Message is IDictionary) {
				this.Errors.Add(JsonSerializer.Serialize(Message));
			} else {
				this.Errors.Add(Message.ToString());
			}
		}
	}
}

