#ifndef CPP_RUNTIME_RUNTIMEREQUEST_H
#define CPP_RUNTIME_RUNTIMEREQUEST_H

#include <string>
#include <json/value.h>
#include <drogon/HttpRequest.h>

namespace runtime
{
    struct RuntimeRequest
    {
        std::string payload;
        Json::Value env;
        Json::Value headers;
    };
}

namespace drogon
{
    template<>
    inline runtime::RuntimeRequest fromRequest(const drogon::HttpRequest &req)
    {
        const std::shared_ptr<Json::Value> &json = req.getJsonObject();
        runtime::RuntimeRequest request;
        if (json)
        {
            request.payload = (*json)["payload"].asString();
            request.env = (*json)["env"];
            request.headers = (*json)["headers"];
        }
        if (request.payload.empty())
        {
            request.payload = "";
        }
        return request;
    }
}
#endif //CPP_RUNTIME_RUNTIMEREQUEST_H
