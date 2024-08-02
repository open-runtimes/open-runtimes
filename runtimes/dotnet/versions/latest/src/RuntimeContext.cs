namespace DotNetRuntime
{
    public class RuntimeContext
    {
        public RuntimeRequest Req { get; set; }
        public RuntimeResponse Res { get; set; }
        public RuntimeLogger Logger { get; set; }

        public RuntimeContext(RuntimeRequest req, RuntimeResponse res, RuntimeLogger logger)
        {
            Req = req;
            Res = res;
            Logger = logger;
        }

        public void Log(object message)
        {
            this.Logger.Write(message, RuntimeLogger.TYPE_LOG);
        }

        public void Error(object message)
        {
            this.Logger.Write(message, RuntimeLogger.TYPE_ERROR);
        }
    }
}
