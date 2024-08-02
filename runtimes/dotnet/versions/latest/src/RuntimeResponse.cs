using System.Text.Json;

namespace DotNetRuntime
{
    public class RuntimeResponse
    {
        public RuntimeOutput Binary(
            byte[] bytes,
            int statusCode = 200,
            Dictionary<string, string>? headers = null
        )
        {
            return new RuntimeOutput(
                bytes,
                statusCode,
                headers ?? new Dictionary<string, string>()
            );
        }

        public RuntimeOutput Send(
            string body,
            int statusCode = 200,
            Dictionary<string, string>? headers = null
        )
        {
            return Text(body, statusCode, headers ?? new Dictionary<string, string>());
        }

        public RuntimeOutput Text(
            string body,
            int statusCode = 200,
            Dictionary<string, string>? headers = null
        )
        {
            return Binary(
                System.Text.Encoding.UTF8.GetBytes(body),
                statusCode,
                headers ?? new Dictionary<string, string>()
            );
        }

        public RuntimeOutput Json(
            Dictionary<string, object?> json,
            int statusCode = 200,
            Dictionary<string, string>? headers = null
        )
        {
            if (headers == null)
            {
                headers = new Dictionary<string, string>();
            }

            headers.Add("content-type", "application/json");
            return Text(JsonSerializer.Serialize(json), statusCode, headers);
        }

        public RuntimeOutput Empty()
        {
            return Text("", 204, new Dictionary<string, string>());
        }

        public RuntimeOutput Redirect(
            String url,
            int statusCode = 301,
            Dictionary<string, string>? headers = null
        )
        {
            if (headers == null)
            {
                headers = new Dictionary<string, string>();
            }

            headers.Add("location", url);

            return Text("", statusCode, headers);
        }
    }
}
