#ifndef CPP_RUNTIME_RUNTIMEREQUEST_H
#define CPP_RUNTIME_RUNTIMEREQUEST_H

#include <string>
#include <json/value.h>
#include <drogon/HttpRequest.h>

namespace runtime
{
    struct RuntimeRequest
    {
        std::string rawBody;
        std::any body;
        Json::Value headers;
        std::string method;
        std::string url;
    };
}
#endif //CPP_RUNTIME_RUNTIMEREQUEST_H
