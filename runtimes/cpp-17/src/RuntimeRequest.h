#ifndef CPP_RUNTIME_RUNTIMEREQUEST_H
#define CPP_RUNTIME_RUNTIMEREQUEST_H

#include <string>
#include <json/value.h>
#include <drogon/HttpRequest.h>

namespace runtime
{
    struct RuntimeRequest
    {
        std::string method;
        std::string scheme;
        std::string host;
        int port;
        std::string path;
        Json::Value query;
        std::string queryString;
        Json::Value headers;
        std::any body;
        std::string bodyString;
        std::string url;
    };
}
#endif //CPP_RUNTIME_RUNTIMEREQUEST_H
