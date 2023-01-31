using System.Collections; 

namespace DotNetRuntime
{
	public class RuntimeContext
	{
		public RuntimeRequest Req { get; set; }
		public RuntimeResponse Res { get; set; }

		public ArrayList _Logs = new ArrayList();
		public ArrayList _Errors = new ArrayList();

		public RuntimeContext(RuntimeRequest Req, RuntimeResponse Res)
		{
			this.Req = Req;
			this.Res = Res;
		}

		public void Log(object Message)
		{
			this._Logs.Add(Message.ToString());
		}

		public void Error(object Message)
		{
			this._Errors.Add(Message.ToString());
		}
	}
}

