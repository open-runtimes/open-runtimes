#include <drogon/drogon.h>
#include <curl/curl.h>
#include "RuntimeResponse.h"
#include "RuntimeRequest.h"
#include "RuntimeOutput.h"
#include "RuntimeContext.h"
#include "{entrypointFile}"
#include <vector>
#include <numeric>

using namespace std;
using namespace runtime;
using namespace drogon;

vector<string> split(const string &s, const char delim) {
    vector<string> result;
    stringstream stream (s);
    string item;

    while (getline(stream, item, delim)) {
        result.push_back(item);
    }

    return result;
}

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

                string secret = req->getHeader("x-open-runtimes-secret");
                if (secret.empty() || secret != std::getenv("OPEN_RUNTIMES_SECRET"))
                {
                    res->setStatusCode(static_cast<HttpStatusCode>(500));
                    res->setBody("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.");
                    callback(res);
                    return;
                }

                const char *method = req->getMethodString();

                string path = req->getPath();
                string queryString = req->getQuery();

                RuntimeRequest runtimeRequest;
                runtimeRequest.method = method;
                runtimeRequest.queryString = queryString;
                runtimeRequest.bodyString = req->getBody();
                runtimeRequest.body = runtimeRequest.bodyString;
                runtimeRequest.path = path;

                string scheme = req->getHeader("x-forwarded-proto");
                if (scheme.empty())
                {
                    scheme = "http";
                }

                const char *defaultPort = "80";
                if (scheme == "https") {
                    defaultPort = "443";
                }

                string host = req->getHeader("host");
                int port = std::stoi(defaultPort);

                if (host.empty())
                {
                    host = "";
                }

                if (host.find(':') != std::string::npos) {
                    vector<string> pair = split(host, ':');

                    if (pair.size() >= 2) {
                        host = pair[0];
                        port = std::stoi(pair[1]);
                    } else {
                        host = host;
                        port = std::stoi(defaultPort);
                    }
                }

                runtimeRequest.scheme = scheme;
                runtimeRequest.host = host;
                runtimeRequest.port = port;

                string url = scheme + "://" + host;

                if(port != std::stoi(defaultPort)) {
                    url += ":" + std::to_string(port);
                }

                url += path;

                if(!queryString.empty()) {
                    url += "?" + queryString;
                }

                runtimeRequest.url = url;

                Json::Value query;
                vector<string> params;

                if(queryString.find('&') != std::string::npos)
                {
                    params = split(queryString, '&');
                } else
                {
                    params.push_back(queryString);
                }

                for (const string &param : params)
                {
                    if(param.find('=') != std::string::npos)
                    {
                        vector<string> pairs = split(param, '=');
                        if(pairs.size() <= 1) {
                            query[pairs[0]] = "";
                        } else {
                            string key = pairs[0];
                            pairs.erase(pairs.begin());

                            auto value = std::accumulate(
                                std::next(pairs.begin()), 
                                pairs.end(), 
                                pairs[0], 
                                [](const std::string &a, const std::string &b) {
                                    return a + "=" + b;
                                }
                            );
                            
                            query[key] = value.c_str();
                        }
                    } else
                    {
                        if(!param.empty()) 
                        {
                            query[param] = "";
                        }
                    }
                }

                if (query.empty()) 
                {
                    Json::Value root;
                    Json::Reader reader;
                    reader.parse("{}", root);
                    query = root;
                }

                runtimeRequest.query = query;

                Json::Value headers;
                for (const auto &header : req->getHeaders())
                {
                    string headerKey = header.first;
                    std::transform(
                        headerKey.begin(),
                        headerKey.end(),
                        headerKey.begin(),
                        [](unsigned char c){ return std::tolower(c); }
                    );

                    if (headerKey.rfind("x-open-runtimes-", 0) != 0)
                    {
                        headers[headerKey] = req->getHeader(header.first);
                    }
                }

                runtimeRequest.headers = headers;

                string contentType = req->getHeader("content-type");
                if(contentType.empty())
                {
                    contentType = "text/plain";
                }

                if (contentType.find("application/json") != std::string::npos)
                {
                    Json::Value bodyRoot;   
                    Json::Reader reader;
                    reader.parse(runtimeRequest.bodyString, bodyRoot); 
                    runtimeRequest.body = bodyRoot;
                }

                RuntimeResponse contextResponse;

                RuntimeContext context;
                context.req = runtimeRequest;
                context.res = contextResponse;

                std::stringstream outBuffer;
                std::stringstream errBuffer;
                std::streambuf *oldOut = std::cout.rdbuf(outBuffer.rdbuf());
                std::streambuf *oldErr = std::cerr.rdbuf(errBuffer.rdbuf());

                RuntimeOutput output;
                try {
                    output = Handler::main(context);
                } catch(const std::exception& e)
                {
                    // TODO: Send trace to context.error()
                    context.error(e.what());
                    output = contextResponse.send("", 500, {});
                }

                if(!outBuffer.str().empty() || !errBuffer.str().empty())
                {
                    context.log("Unsupported log detected. Use context.log() or context.error() for logging.");
                }

                std::cout.rdbuf(oldOut);
                std::cerr.rdbuf(oldErr);

                for (const string &key : output.headers.getMemberNames())
                {
                    string headerKey = key;
                    std::transform(
                        headerKey.begin(),
                        headerKey.end(),
                        headerKey.begin(),
                        [](unsigned char c){ return std::tolower(c); }
                    );

                    if (headerKey.rfind("x-open-runtimes-", 0) != 0)
                    {
                        res->addHeader(headerKey, output.headers[key].asString());
                    }
                }

                CURL *curl = curl_easy_init();

                if(!context.logs.empty())
                {
                    string logsString = std::accumulate(
                        std::next(context.logs.begin()), 
                        context.logs.end(), 
                        context.logs[0], 
                        [](const string &a, const string &b) {
                            return a + "\n" + b;
                        }
                    );

                    char *logsEncoded = curl_easy_escape(
                        curl,
                        logsString.c_str(),
                        logsString.length()
                    );

                    res->addHeader("x-open-runtimes-logs", logsEncoded);
                } else {
                    res->addHeader("x-open-runtimes-logs", "");
                }

                if(!context.errors.empty())
                {
                    string errorsString = std::accumulate(
                        std::next(context.errors.begin()), 
                        context.errors.end(), 
                        context.errors[0], 
                        [](const string &a, const string &b) {
                            return a + "\n" + b;
                        }
                    );

                    char *errorsEncoded = curl_easy_escape(
                        curl,
                        errorsString.c_str(),
                        errorsString.length()
                    );

                    res->addHeader("x-open-runtimes-errors", errorsEncoded);
                } else {
                    res->addHeader("x-open-runtimes-errors", "");
                }

                res->setStatusCode(static_cast<HttpStatusCode>(output.statusCode));
                res->setBody(output.body);
                callback(res);
            },
            {Get, Post, Put, Patch, Delete, Options})
        .run();

    return 0;
}

