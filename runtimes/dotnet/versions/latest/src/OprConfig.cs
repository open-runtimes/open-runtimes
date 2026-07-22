using System;
using System.Collections.Generic;
using System.Text.Json;

namespace DotNetRuntime
{
    public static class OprConfig
    {
        public static readonly String Secret =
            Environment.GetEnvironmentVariable("OPEN_RUNTIMES_SECRET") ?? "";
        public static readonly String Env =
            Environment.GetEnvironmentVariable("OPEN_RUNTIMES_ENV") ?? "";
        public static readonly Dictionary<string, string> Headers = LoadHeaders();

        private static Dictionary<string, string> LoadHeaders()
        {
            var headers = new Dictionary<string, string>();
            var json = Environment.GetEnvironmentVariable("OPEN_RUNTIMES_HEADERS");

            if (string.IsNullOrEmpty(json))
            {
                return headers;
            }

            try
            {
                var parsed =
                    JsonSerializer.Deserialize<Dictionary<string, object>>(json)
                    ?? new Dictionary<string, object>();
                foreach (KeyValuePair<string, object> entry in parsed)
                {
                    headers[entry.Key.ToLower()] = Convert.ToString(entry.Value) ?? "";
                }
            }
            catch (JsonException)
            {
                // Invalid JSON results in no enforced headers
            }

            return headers;
        }
    }
}
