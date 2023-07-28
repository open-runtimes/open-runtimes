#include <drogon/drogon.h>
#include <curl/curl.h>
#include "RuntimeResponse.h"
#include "RuntimeRequest.h"
#include "RuntimeOutput.h"
#include "RuntimeContext.h"
#include "{entrypointFile}"
#include <vector>
#include <numeric>

std::vector<std::string> split(const std::string &s, const char delim) {
    std::vector<std::string> result;
    std::stringstream stream (s);
    std::string item;

    while (getline(stream, item, delim)) {
        result.push_back(item);
    }

    return result;
}

int main()
{
    drogon::app()
        .addListener("0.0.0.0", 3000)
        .registerHandlerViaRegex(
            "/.*",
            [](const drogon::HttpRequestPtr &req,
               std::function<void(const drogon::HttpResponsePtr &)> &&callback)
            {
                const std::shared_ptr<drogon::HttpResponse> res = drogon::HttpResponse::newHttpResponse();

                int timeout = -1;
                std::string timeout_header = req->getHeader("x-open-runtimes-timeout");
                if (!timeout_header.empty())
                {
                    bool invalid = false;

                    try
                    {
                        timeout = std::stoi(timeout_header);
                    } catch (const std::invalid_argument& ia)
                    {
                        invalid = true;
                    }

                    if (invalid || timeout == 0)
                    {
                        res->setStatusCode(static_cast<drogon::HttpStatusCode>(500));
                        res->setBody("Header \"x-open-runtimes-timeout\" must be an integer greater than 0.");
                        callback(res);
                        return;
                    }
                }

                std::string secret = req->getHeader("x-open-runtimes-secret");
                if (secret.empty() || secret != std::getenv("OPEN_RUNTIMES_SECRET"))
                {
                    res->setStatusCode(static_cast<drogon::HttpStatusCode>(500));
                    res->setBody("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.");
                    callback(res);
                    return;
                }

                std::string method = req->getMethodString();
                std::string path = req->getPath();
                std::string queryString = req->getQuery();

                runtime::RuntimeRequest runtimeRequest;
                runtimeRequest.method = method;
                runtimeRequest.queryString = queryString;
                runtimeRequest.bodyRaw = req->getBody();
                runtimeRequest.body = runtimeRequest.bodyRaw;
                runtimeRequest.path = path;

                std::string scheme = req->getHeader("x-forwarded-proto");
                if (scheme.empty())
                {
                    scheme = "http";
                }

                const char *defaultPort = "80";
                if (scheme == "https") {
                    defaultPort = "443";
                }

                std::string host = req->getHeader("host");
                int port = std::stoi(defaultPort);

                if (host.empty())
                {
                    host = "";
                }

                if (host.find(':') != std::string::npos) {
                    std::vector<std::string> pair = split(host, ':');

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

                std::string url = scheme + "://" + host;

                if(port != std::stoi(defaultPort)) {
                    url += ":" + std::to_string(port);
                }

                url += path;

                if(!queryString.empty()) {
                    url += "?" + queryString;
                }

                runtimeRequest.url = url;

                Json::Value query;
                std::vector<std::string> params;

                if(queryString.find('&') != std::string::npos)
                {
                    params = split(queryString, '&');
                } else
                {
                    params.push_back(queryString);
                }

                for (const std::string &param : params)
                {
                    if(param.find('=') != std::string::npos)
                    {
                        std::vector<std::string> pairs = split(param, '=');
                        if(pairs.size() <= 1)
                        {
                            query[pairs[0]] = "";
                        } else
                        {
                            std::string key = pairs[0];
                            pairs.erase(pairs.begin());

                            std::string value = std::accumulate(
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
                    std::string headerKey = header.first;
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

                std::string contentType = req->getHeader("content-type");
                if(contentType.empty())
                {
                    contentType = "text/plain";
                }

                if (contentType.find("application/json") != std::string::npos)
                {
                    Json::Value bodyRoot;   
                    Json::Reader reader;
                    reader.parse(runtimeRequest.bodyRaw, bodyRoot); 
                    runtimeRequest.body = bodyRoot;
                }

                runtime::RuntimeResponse contextResponse;
                runtime:: RuntimeContext context;

                context.req = runtimeRequest;
                context.res = contextResponse;

                std::stringstream outBuffer;
                std::stringstream errBuffer;
                std::streambuf *oldOut = std::cout.rdbuf(outBuffer.rdbuf());
                std::streambuf *oldErr = std::cerr.rdbuf(errBuffer.rdbuf());

                runtime::RuntimeOutput output;

                try {
                    if (timeout != -1)
                    {
                        std::promise<runtime::RuntimeOutput> promise;
                        std::future<runtime::RuntimeOutput> future = promise.get_future();
                        std::thread thread([&context](std::promise<runtime::RuntimeOutput> promise) {
                            try {
                                promise.set_value(runtime::Handler::main(context));
                            } catch (...) {
                                promise.set_exception(std::current_exception());
                            }
                        }, std::move(promise));

                        std::future<void> waiter = std::async(std::launch::async, &std::thread::join, &thread);

                        if (waiter.wait_for(std::chrono::seconds(timeout)) == std::future_status::timeout) {
                            context.error("Execution timed out.");
                            output = context.res.send("", 500, {});
                            pthread_cancel(thread.native_handle());
                        } else {
                            output = future.get();
                        }
                    } else
                    {
                        output = runtime::Handler::main(context);
                    }
                } catch(const std::exception& e)
                {
                    context.error(e.what());
                    output = contextResponse.send("", 500, {});
                }

                if(!outBuffer.str().empty() || !errBuffer.str().empty())
                {
                    context.log("Unsupported log detected. Use context.log() or context.error() for logging.");
                }

                std::cout.rdbuf(oldOut);
                std::cerr.rdbuf(oldErr);

                for (const std::string &key : output.headers.getMemberNames())
                {
                    std::string headerKey = key;
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
                    std::string logsString = std::accumulate(
                        std::next(context.logs.begin()), 
                        context.logs.end(), 
                        context.logs[0], 
                        [](const std::string &a, const std::string &b) {
                            return a + "\n" + b;
                        }
                    );

                    std::string logsEncoded = curl_easy_escape(
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
                    std::string errorsString = std::accumulate(
                        std::next(context.errors.begin()), 
                        context.errors.end(), 
                        context.errors[0], 
                        [](const std::string &a, const std::string &b) {
                            return a + "\n" + b;
                        }
                    );

                    std::string errorsEncoded = curl_easy_escape(
                        curl,
                        errorsString.c_str(),
                        errorsString.length()
                    );

                    res->addHeader("x-open-runtimes-errors", errorsEncoded);
                } else {
                    res->addHeader("x-open-runtimes-errors", "");
                }

                res->setStatusCode(static_cast<drogon::HttpStatusCode>(output.statusCode));
                res->setBody(output.body);
                callback(res);
            },
            {
                drogon::Get,
                drogon::Post,
                drogon::Put,
                drogon::Patch,
                drogon::Delete,
                drogon::Options
            })
        .run();

    return 0;
}

