using System.Collections; 

namespace DotNetRuntime
{
	public class RuntimeContext
	{
		public RuntimeRequest req { get; set; }
		public RuntimeResponse res { get; set; }

		public ArrayList _logs = new ArrayList();
		public ArrayList _errors = new ArrayList();

		public RuntimeContext(RuntimeRequest req, RuntimeResponse res)
		{
			this.req = req;
			this.res = res;
		}

		public void log(object message) {
			this._logs.Add(message.ToString());
		}

		public void error(object message) {
			this._errors.Add(message.ToString());
		}
	}
}

