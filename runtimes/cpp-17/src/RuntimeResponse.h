#ifndef CPP_RUNTIME_RUNTIMERESPONSE_H
#define CPP_RUNTIME_RUNTIMERESPONSE_H

#include <string>
#include <json/value.h>
#include "RuntimeOutput.h"

namespace runtime
{
    class RuntimeResponse
    {
        public:
            RuntimeOutput send(const std::string &body, const int statusCode = 200, Json::Value headers = {})
            {
                RuntimeOutput output;
                output.body = body;
                output.statusCode = statusCode;
                output.headers = headers;
                return output;
            }

            RuntimeOutput json(const Json::Value &json, const int statusCode = 200, Json::Value headers = {})
            {
                if (headers["content-type"].asString().empty()) {
                    headers["content-type"] = "application/json";
                }
                return this->send(json.toStyledString(), statusCode, headers);
            }

            RuntimeOutput empty()
            {
                return this->send("", 204, {});
            }

            RuntimeOutput redirect(const std::string &url, const int statusCode = 301, Json::Value headers = {})
            {
                headers["location"] = url;
                return this->send("", statusCode, headers);
            }
    };
}

#endif //CPP_RUNTIME_RUNTIMERESPONSE_H
