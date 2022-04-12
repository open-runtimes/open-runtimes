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
                    httpResponse->setStatusCode(static_cast<HttpStatusCode>(500))
                    httpResponse->setBody("Unauthorized");
                    callback(httpResponse);
                    return;
                }

                RuntimeResponse &runtimeResponse = *new RuntimeResponse();

                try {
                    Wrapper::main(runtimeRequest, runtimeResponse);
                    httpResponse->setStatusCode(static_cast<HttpStatusCode>(runtimeResponse.statusCode));
                    httpResponse->setBody(runtimeResponse.data);
                } catch (const std::exception& e) {
                    httpResponse->setStatusCode(static_cast<HttpStatusCode>(500));
                    httpResponse->setBody(e.what());
                }
                
                callback(httpResponse);
            },
            {Post})
        .run();
    return 0;
}

