using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.Extensions.Logging;

namespace DotNetRuntime
{
    [ApiController]
    [Route("/")]
    public class Controller : ControllerBase
    {
        [HttpPost]
        public async Task<IActionResult> Execute(
            [FromHeader(Name = "x-internal-challenge")] string? challengeId,
            [FromBody] RuntimeRequest? request)
        {
            if (challengeId == null 
                || challengeId != Environment.GetEnvironmentVariable("INTERNAL_RUNTIME_KEY"))
            {
                return Problem(detail: "Unauthorized");
            }

            var originalOut = Console.Out;
            var originalErr = Console.Error;
            var outString = new StringBuilder();
            var errString = new StringBuilder();

            try
            {
                var codeWrapper = new Wrapper();
                var req = request ?? new RuntimeRequest(); 
                var res = new RuntimeResponse();

                var outWriter = new StringWriter(outString);
                var errWriter = new StringWriter(errString);

                Console.SetOut(outWriter);
                Console.SetError(errWriter);

                var response = await codeWrapper.Main(req, res);
                var output = new Dictionary<string, object?>()
                {
                    { "response", response.Data },
                    { "stdout", outString.ToString() }
                };
                return Content(JsonSerializer.Serialize(output), "application/json");
            }
            catch (Exception e)
            {
                Console.Error.Write(e);
                var output = new Dictionary<string, object?>()
                {
                    { "stderr", errString.ToString() },
                    { "stdout", outString.ToString() }
                };
                return Content(JsonSerializer.Serialize(output), "application/json");
            }
            finally
            {
                Console.SetOut(originalOut);
                Console.SetError(originalErr);
            }
        }
    }
}

