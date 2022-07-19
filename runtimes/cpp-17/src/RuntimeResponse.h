#ifndef CPP_RUNTIME_RUNTIMERESPONSE_H
#define CPP_RUNTIME_RUNTIMERESPONSE_H

#include <string>
#include <json/value.h>
#include "RuntimeRequest.h"

namespace runtime
{
    struct RuntimeResponse
    {
        std::string stringValue;
        Json::Value jsonValue;
        int statusCode{200};

        RuntimeResponse &json(const Json::Value &json, int code = 200)
        {
            jsonValue = json;
            statusCode = code;
            return *this;
        }

        RuntimeResponse &send(const std::string &string, int code = 200)
        {
            stringValue = string;
            statusCode = code;
            return *this;
        }
    };
}

#endif //CPP_RUNTIME_RUNTIMERESPONSE_H
