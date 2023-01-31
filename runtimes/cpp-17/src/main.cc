#include <drogon/drogon.h>
#include "RuntimeResponse.h"
#include "RuntimeRequest.h"
#include "RuntimeOutput.h"
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
                res->setStatusCode(static_cast<HttpStatusCode>(201));
                res->setBody("Heyyy");
                callback(res);

                /*
                const std::shared_ptr<HttpResponse> res = HttpResponse::newHttpResponse();

                if (req->getHeader("x-internal-challenge") != std::getenv("OPEN_RUNTIMES_SECRET"))
                {
                    res->setStatusCode(static_cast<HttpStatusCode>(500));
                    res->setBody("Unauthorized");
                    callback(res);
                    return;
                }

                const std::shared_ptr<RuntimeResponse> runtimeResponse(new RuntimeResponse());

                std::stringstream outbuffer;
                std::stringstream errbuffer;
                std::streambuf *oldout = std::cout.rdbuf(outbuffer.rdbuf());
                std::streambuf *olderr = std::cerr.rdbuf(errbuffer.rdbuf());

                try {
                    Wrapper::main(runtimeRequest, *runtimeResponse);
                    res->setStatusCode(static_cast<HttpStatusCode>(runtimeResponse->statusCode));

                    Json::Value output;
                    if (runtimeResponse->stringValue.length() != 0)
                    {
                        output["response"] = runtimeResponse->stringValue;
                    }
                    else
                    {
                        output["response"] = runtimeResponse->jsonValue;
                    }
                    output["stdout"] = outbuffer.str();
                    res->setBody(output.toStyledString());
                } catch (const std::exception& e) {
                    Json::Value output;
                    output["stderr"] = errbuffer.str() + e.what();
                    output["stdout"] = outbuffer.str();
                    res->setStatusCode(static_cast<HttpStatusCode>(500));
                    res->setBody(output.toStyledString());
                }

                std::cout.rdbuf(oldout);
                std::cerr.rdbuf(olderr);

                callback(res);
                */
            },
            {Post})
        .run();
    return 0;
}

