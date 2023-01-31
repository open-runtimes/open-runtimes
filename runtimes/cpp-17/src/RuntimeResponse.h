#ifndef CPP_RUNTIME_RUNTIMERESPONSE_H
#define CPP_RUNTIME_RUNTIMERESPONSE_H

#include <string>
#include <json/value.h>
#include "RuntimeOutput.h"

namespace runtime
{
    struct RuntimeResponse
    {
        RuntimeOutput &send(const std::string &body, int statusCode = 200, Json::Value headers = {})
        {
            RuntimeOutput* output;
            output->body = body;
            output->statusCode = statusCode;
            output->headers = headers;
            return output;
        }

        RuntimeOutput &json(const Json::Value &json, int statusCode = 200, Json::Value headers = {})
        {
            headers["content-type"] = "application/json";
            return this->send(json.toStyledString(), statusCode, headers);
        }

        // empty, redirect
    };
}

#endif //CPP_RUNTIME_RUNTIMERESPONSE_H
