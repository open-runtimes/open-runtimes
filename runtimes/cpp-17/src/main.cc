#include <drogon/drogon.h>
#include "RuntimeResponse.h"
#include "RuntimeRequest.h"
#include "Wrapper.h"

using namespace std;
using namespace runtime;
using namespace drogon;

int main()
{
    app()
        .addListener("0.0.0.0", 3000)
        .registerHandler(
            "/",
            [](const HttpRequestPtr &req,
               function<void(const HttpResponsePtr &)> &&callback, const RuntimeRequest &runtimeRequest)
            {
                const std::shared_ptr<HttpResponse> res = HttpResponse::newHttpResponse();

                if (req->getHeader("x-internal-challenge") != std::getenv("INTERNAL_RUNTIME_KEY"))
                {
                    res->setStatusCode(static_cast<HttpStatusCode>(500));
                    res->setBody("Unauthorized");
                    callback(res);
                    return;
                }

                const std::shared_ptr<RuntimeResponse> runtimeResponse(new RuntimeResponse());

                try {
                    Wrapper::main(runtimeRequest, *runtimeResponse);
                    res->setStatusCode(static_cast<HttpStatusCode>(runtimeResponse->statusCode));
                    res->setBody(runtimeResponse->data);
                } catch (const std::exception& e) { 
                    res->setStatusCode(static_cast<HttpStatusCode>(500));
                    res->setBody(e.what());
                }
                
                callback(res);
            },
            {Post})
        .run();
    return 0;
}

