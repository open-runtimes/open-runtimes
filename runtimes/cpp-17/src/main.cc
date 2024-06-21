#include <drogon/drogon.h>
#include <curl/curl.h>
#include "RuntimeResponse.h"
#include "RuntimeRequest.h"
#include "RuntimeOutput.h"
#include "RuntimeContext.h"
#include "RuntimeLogger.h"
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
                std::shared_ptr<runtime::RuntimeLogger> logger = std::make_shared<runtime::RuntimeLogger>(req->getHeader("x-open-runtimes-logging"), req->getHeader("x-open-runtimes-log-id"));

                try {
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

                    if(std::getenv("OPEN_RUNTIMES_SECRET") != nullptr) {
                        std::string serverSecret(std::getenv("OPEN_RUNTIMES_SECRET"));
                        std::string secret = req->getHeader("x-open-runtimes-secret");

                        if(serverSecret != "" && secret != serverSecret) {
                            res->setStatusCode(static_cast<drogon::HttpStatusCode>(500));
                            res->setBody("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.");
                            callback(res);
                            return;
                        }
                    }

                    std::string method = req->getMethodString();
                    std::string path = req->getPath();
                    std::string queryString = req->getQuery();

                    runtime::RuntimeRequest runtimeRequest;
                    runtimeRequest.method = method;
                    runtimeRequest.queryString = queryString;

                    runtimeRequest.bodyText = req->getBody();
                    runtimeRequest.bodyRaw = runtimeRequest.bodyText;
                    std::vector<std::byte> bodyBytes;
                    bodyBytes.reserve(runtimeRequest.bodyText.size());
                    std::transform(std::begin(runtimeRequest.bodyText), std::end(runtimeRequest.bodyText), std::back_inserter(bodyBytes), [](char c){
                        return std::byte(c);
                    });
                    runtimeRequest.bodyBinary = bodyBytes;

                    if(runtimeRequest.bodyText.empty())
                    {
                        Json::Value bodyRootEmpty;
                        runtimeRequest.bodyJson = bodyRootEmpty;
                    } else {
                        Json::Value bodyRoot;   
                        Json::Reader reader;
                        bool parsingResult = reader.parse(runtimeRequest.bodyText, bodyRoot);

                        if(!parsingResult)
                        {
                            Json::Value bodyRootEmpty;
                            runtimeRequest.bodyJson = bodyRootEmpty;
                        } else {
                            runtimeRequest.bodyJson = bodyRoot;
                        }
                    }

                    std::string contentType = req->getHeader("content-type");
                    if(contentType.empty())
                    {
                        contentType = "text/plain";
                    }

                    std::transform(
                        contentType.begin(),
                        contentType.end(),
                        contentType.begin(),
                        [](unsigned char c){ return std::tolower(c); }
                    );

                    if (contentType.rfind("application/json", 0) == 0) {
                        runtimeRequest.body = runtimeRequest.bodyJson;
                    } else {
                        bool isBinary = false;

                        std::vector<std::string> binaryTypes = {"application/", "audio/", "font/", "image/", "video/"};
                        for (const std::string& binaryType : binaryTypes) {
                            if (contentType.rfind(binaryType, 0) == 0) {
                                runtimeRequest.body = runtimeRequest.bodyBinary;
                                isBinary = true;
                                break;
                            }
                        }

                        if(!isBinary) {
                            runtimeRequest.body = runtimeRequest.bodyText;
                        }
                    }
                    
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

                    std::vector<std::string> cookieHeaders;
                    for (const auto &cookie : req->getCookies())
                    {
                        cookieHeaders.push_back(cookie.first + "=" + req->getCookie(cookie.first));
                    }

                    // Reverse because it's coming in exact opposide order from HTTP library
                    std::reverse(cookieHeaders.begin(), cookieHeaders.end());

                    if(!(cookieHeaders.empty()))
                    {
                        std::string cookieHeadersString = std::accumulate(
                            std::next(cookieHeaders.begin()), 
                            cookieHeaders.end(), 
                            cookieHeaders[0], 
                            [](const std::string &a, const std::string &b) {
                                return a + "; " + b;
                            }
                        );

                        headers["cookie"] = cookieHeadersString;
                    }

                    char* serverHeadersChar = std::getenv("OPEN_RUNTIMES_HEADERS");
                    if(serverHeadersChar != nullptr) {
                        std::string serverHeadersString(serverHeadersChar);

                        if(serverHeadersString.empty()) {
                            serverHeadersString = "{}";
                        }

                        Json::Value serverHeaders;   
                        Json::Reader serverHeadersReader;
                        bool parsingResult = serverHeadersReader.parse(serverHeadersString, serverHeaders);
                        if(!parsingResult)
                        {
                            throw std::runtime_error("Invalid JSON body.");
                        }

                        for (const std::string &key : serverHeaders.getMemberNames())
                        {
                            std::string headerKey = key;
                            std::transform(
                                headerKey.begin(),
                                headerKey.end(),
                                headerKey.begin(),
                                [](unsigned char c){ return std::tolower(c); }
                            );

                            auto value = serverHeaders[key];
                            headers[headerKey] = value.asString();
                        }
                    }

                    runtimeRequest.headers = headers;

                    runtime::RuntimeResponse contextResponse;
                    runtime:: RuntimeContext context;

                    context.req = runtimeRequest;
                    context.res = contextResponse;
                    context.logger = logger;

                    logger->overrideNativeLogs();

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
                                output = context.res.text("", 500, {});
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

                    logger->revertNativeLogs();

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
                    
                    std::string contentTypeHeader = res->getHeader("content-type");
                    if (contentTypeHeader.empty())
                    {
                        contentTypeHeader = "text/plain";
                    }

                    std::transform(
                        contentTypeHeader.begin(),
                        contentTypeHeader.end(),
                        contentTypeHeader.begin(),
                        [](unsigned char c){ return std::tolower(c); }
                    );

                    if (contentTypeHeader.find("multipart/") == std::string::npos &&
                        contentTypeHeader.find("charset=") == std::string::npos)
                    {
                        contentTypeHeader.append("; charset=utf-8");
                    }

                    res->addHeader("content-type", contentTypeHeader);

                    logger->end();
                    res->addHeader("x-open-runtimes-log-id", logger->id);

                    res->setStatusCode(static_cast<drogon::HttpStatusCode>(output.statusCode));

                    std::string outputBodyString;
                    for (const auto& byte : output.body) {
                        outputBodyString += static_cast<char>(byte);
                    }

                    res->setBody(outputBodyString);
                    callback(res);
                } catch(const std::exception& e)
                {
                    const std::shared_ptr<drogon::HttpResponse> res = drogon::HttpResponse::newHttpResponse();

                    logger->write(e.what(), "error");
                    logger->end();

                    res->addHeader("x-open-runtimes-log-id", logger->id);

                    res->setStatusCode(static_cast<drogon::HttpStatusCode>(500));
                    res->setBody("");
                    callback(res);
                }
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

