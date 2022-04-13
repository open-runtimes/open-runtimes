namespace DotNetRuntime
{
	public class RuntimeRequest
	{
		public string Payload { get; set; } = "";

		public Dictionary<string, string> Env { get; set; } =
			new Dictionary<string, string>();

		public Dictionary<string, string> Headers { get; set; } =
			new Dictionary<string, string>();
	}
}

