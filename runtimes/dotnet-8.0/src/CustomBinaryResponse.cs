using System.Text;

namespace DotNetRuntime
{
    class CustomBinaryResponse : IResult
    {

        public Task ExecuteAsync(HttpContext httpContext)
        {
            var body = Encoding.UTF8.GetBytes("");
            
            return httpContext.Response.Body.WriteAsync(body,0 , body.Length);
        }
    }
}