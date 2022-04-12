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
                if (req->getHeader("x-internal-challenge") != getenv("INTERNAL_RUNTIME_KEY"))
                {
                    const shared_ptr<HttpResponse> resp = HttpResponse::newCustomHttpResponse(
                        RuntimeResponse::unauthorized());
                    callback(resp);
                    return;
                }

                RuntimeResponse &runtimeResponse = *new RuntimeResponse();
                const shared_ptr<HttpResponse> httpResponse = HttpResponse::newHttpResponse();

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

