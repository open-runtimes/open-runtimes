using System;
using System.Collections.Generic;
using System.Linq;
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

            try
            {
                var codeWrapper = new Wrapper();
                var req = request ?? new RuntimeRequest(); 
                var res = new RuntimeResponse();
                var response = await codeWrapper.Main(req, res);

                return Content(response.Data, "application/json");
            }
            catch (Exception e)
            {
                return Problem(title: e.Message, detail: e.StackTrace);
            }
        }
    }
}

