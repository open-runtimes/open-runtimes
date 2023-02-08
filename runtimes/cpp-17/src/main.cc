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
        .registerHandlerViaRegex(
            "/.*",
            [](const HttpRequestPtr &req,
               function<void(const HttpResponsePtr &)> &&callback)
            {
                const std::shared_ptr<HttpResponse> res = HttpResponse::newHttpResponse();

                auto secret = req->getHeader("x-open-runtimes-secret");
                if (secret.empty() || secret != std::getenv("OPEN_RUNTIMES_SECRET"))
                {
                    res->setStatusCode(static_cast<HttpStatusCode>(500));
                    res->setBody("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.");
                    callback(res);
                    return;
                }

                auto method = req->getMethodString();

                RuntimeRequest contextRequest;
                contextRequest.method = method;
                contextRequest.queryString = req->getQuery();
                contextRequest.bodyString = req->getBody();
                contextRequest.body = contextRequest.bodyString;
                contextRequest.path = req->getPath();

                // TODO: scheme, host, port, url

                Json::Value query;
                // TODO: Fill query from queryString
                /*
                for(const param of queryString.split('&')) {
                    const [ key, value ] = param.split('=');

                    if(key) {
                        query[key] = value;
                    }
                }
                */
                contextRequest.query = query;

                Json::Value headers;
                for(const auto header : req->getHeaders())
                {
                    auto headerValue = req->getHeader(header.first);
                    std::transform(headerValue.begin(), headerValue.end(), headerValue.begin(), [](unsigned char c){ return std::tolower(c); });

                    if (headerValue.rfind("x-open-runtimes-", 0) != 0)
                    {
                        headers[header.first] = headerValue;
                    }
                }

                contextRequest.headers = headers;

                auto contentType = req->getHeader("content-type");
                if(contentType.empty())
                {
                    contentType = "text/plain";
                }

                if (contentType.find("application/json") != std::string::npos)
                {
                    Json::Value bodyRoot;   
                    Json::Reader reader;
                    reader.parse(contextRequest.bodyString.c_str(), bodyRoot); 
                    contextRequest.body = bodyRoot;
                }

                RuntimeResponse contextResponse;

                RuntimeContext context;
                context.req = contextRequest;
                context.res = contextResponse;

                std::stringstream outbuffer;
                std::stringstream errbuffer;
                std::streambuf *oldout = std::cout.rdbuf(outbuffer.rdbuf());
                std::streambuf *olderr = std::cerr.rdbuf(errbuffer.rdbuf());

                RuntimeOutput output;
                try {
                    output = Wrapper::main(context);
                } catch(const std::exception& e) {
                    context.error(e.what());
                    output = contextResponse.send("", 500, {});
                }

                /*
                Should never be null. If somehow is, uncomment:
                if(output == NULL) {
                    context.error("Return statement missing. return context.res.empty() if no response is expected.");
                    output = contextResponse.send("", 500, {});
                }
                */

                if(!outbuffer.str().empty() || !errbuffer.str().empty()) {
                    context.log("Unsupported log noticed. Use context.log() or context.error() for logging.");
                }

                std::cout.rdbuf(oldout);
                std::cerr.rdbuf(olderr);

                // TODO: set output.headers
                // TODO: set x-open-runtimes-logs, x-open-runtimes-errors; urlencode
                
                // TODO: Set response headers

                res->setStatusCode(static_cast<HttpStatusCode>(output.statusCode));
                res->setBody(output.body);
                callback(res);
            },
            {Post, Get, Put, Patch, Delete, Options})
        .run();
    return 0;
}

