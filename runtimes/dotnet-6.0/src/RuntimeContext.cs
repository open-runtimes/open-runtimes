using System.Collections; 

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
			this.Logs.Add(Message.ToString());
		}

		public void Error(object Message)
		{
			this.Errors.Add(Message.ToString());
		}
	}
}

