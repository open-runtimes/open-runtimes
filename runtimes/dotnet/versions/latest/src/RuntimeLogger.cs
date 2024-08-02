using System;
using System.Collections;
using System.Collections.Generic;
using System.Text;
using System.Text.Json;

namespace DotNetRuntime
{
    public class RuntimeLogger
    {
        public static String TYPE_LOG = "log";
        public static String TYPE_ERROR = "error";

        public String id = "";
        private bool enabled = false;
        private bool includesNativeInfo = false;

        private StreamWriter? streamLogs = null;
        private StreamWriter? streamErrors = null;

        private TextWriter? nativeLogsCache = null;
        private TextWriter? nativeErrorsCache = null;

        private StringBuilder? customStdStream = null;

        public RuntimeLogger(String? status, String? id)
        {
            if (string.IsNullOrEmpty(status) || status == "enabled")
            {
                this.enabled = true;
            }
            else
            {
                this.enabled = false;
            }

            if (this.enabled)
            {
                if (string.IsNullOrEmpty(id))
                {
                    String? serverEnv = Environment.GetEnvironmentVariable("OPEN_RUNTIMES_ENV");
                    if (!string.IsNullOrEmpty(serverEnv) && serverEnv == "development")
                    {
                        this.id = "dev";
                    }
                    else
                    {
                        this.id = this.GenerateId();
                    }
                }
                else
                {
                    this.id = id;
                }

                // Log stream
                this.streamLogs = new StreamWriter("/mnt/logs/" + this.id + "_logs.log", true);
                this.streamErrors = new StreamWriter("/mnt/logs/" + this.id + "_errors.log", true);
            }
        }

        public void Write(object message, String? type = null, bool native = false)
        {
            if (string.IsNullOrEmpty(type))
            {
                type = RuntimeLogger.TYPE_LOG;
            }

            if (this.enabled == false)
            {
                return;
            }

            if (native && !this.includesNativeInfo)
            {
                this.includesNativeInfo = true;
                this.Write(
                    "Native logs detected. Use context.Log() or context.Error() for better experience.",
                    type,
                    native
                );
            }

            var stream = this.streamLogs;

            if (type == RuntimeLogger.TYPE_ERROR)
            {
                stream = this.streamErrors;
            }

            String stringLog = "";
            if (message is IList || message is IDictionary)
            {
                stringLog = JsonSerializer.Serialize(message);
            }
            else
            {
                stringLog = message.ToString() ?? "";
            }

            if (stream != null)
            {
                stream.WriteLine(stringLog);
            }
        }

        public void End()
        {
            if (!this.enabled)
            {
                return;
            }

            this.enabled = true;

            if (this.streamLogs != null)
            {
                this.streamLogs.Close();
            }
            if (this.streamErrors != null)
            {
                this.streamErrors.Close();
            }
        }

        public void OverrideNativeLogs()
        {
            this.nativeLogsCache = Console.Out;
            this.nativeErrorsCache = Console.Error;
            this.customStdStream = new StringBuilder();
            var customStdWriter = new StringWriter(this.customStdStream);
            Console.SetOut(customStdWriter);
            Console.SetError(customStdWriter);
        }

        public void RevertNativeLogs()
        {
            if (this.nativeLogsCache != null)
            {
                Console.SetOut(this.nativeLogsCache);
            }

            if (this.nativeErrorsCache != null)
            {
                Console.SetError(this.nativeErrorsCache);
            }

            if (this.customStdStream != null)
            {
                if (!string.IsNullOrEmpty(this.customStdStream.ToString()))
                {
                    this.Write(this.customStdStream.ToString(), RuntimeLogger.TYPE_LOG, true);
                }
            }
        }

        private String GenerateId(int padding = 7)
        {
            var now = DateTime.UtcNow;
            var epoch = (now - new DateTime(1970, 1, 1));
            var sec = (long)epoch.TotalSeconds;
            var usec = (long)((epoch.TotalMilliseconds * 1000) % 1000);
            var baseId = sec.ToString("x") + usec.ToString("x").PadLeft(5, '0');

            var random = new Random();
            var randomPadding = "";

            for (int i = 0; i < padding; i++)
            {
                var randomHexDigit = random.Next(0, 16).ToString("x");
                randomPadding += randomHexDigit;
            }

            return baseId + randomPadding;
        }
    }
}
