using System.Collections.Generic;

namespace DotNetRuntime
{   public class RuntimeResponse
    {
        public object Data { get; set; }
        public int StatusCode { get; set; }

        public RuntimeResponse(
            string data = "",
            int statusCode = 200)
        {
            Data = data;
            StatusCode = statusCode;
        }

        public RuntimeResponse Send(
            string data,
            int statusCode = 200)
        {
            Data = data;
            StatusCode = statusCode;
            return this;
        }

        public RuntimeResponse Json(
            Dictionary<string, object?> data,
            int statusCode = 200)
        {
            Data = data;
            StatusCode = statusCode;
            return this;
        }
    }
}
