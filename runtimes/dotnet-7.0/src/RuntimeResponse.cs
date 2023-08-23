using System;
using System.Collections.Generic;
using System.Text.Json;

namespace DotNetRuntime {
    public class RuntimeResponse
    {
        public RuntimeOutput Send(string body, int statusCode = 200, Dictionary<string, string>? headers = null)
        {
            if(headers == null) {
                headers = new Dictionary<string,string>();
            }

            if (!headers.ContainsKey("content-type"))
            {
                headers.Add("content-type", "text/plain");
            }

            return new RuntimeOutput(
                body,
                statusCode,
                headers);
        }

        public RuntimeOutput Json(Dictionary<string, object?> json, int statusCode = 200, Dictionary<string, string>? headers = null)
        {
            if(headers == null) {
                headers = new Dictionary<string,string>();
            }

            if (!headers.ContainsKey("content-type"))
            {
                headers.Add("content-type", "application/json");
            }

            return Send(JsonSerializer.Serialize(json), statusCode, headers);
        }

        public RuntimeOutput Empty()
        {
            return new RuntimeOutput("", 204, new Dictionary<string,string>());
        }

        public RuntimeOutput Redirect(String url, int statusCode = 301, Dictionary<string, string>? headers = null)
        {
            if(headers == null) {
                headers = new Dictionary<string,string>();
            }

            headers.Add("location", url);

            return Send("", statusCode, headers);
        }
    }
}