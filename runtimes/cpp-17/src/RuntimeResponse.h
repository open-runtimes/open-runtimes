#ifndef CPP_RUNTIME_RUNTIMERESPONSE_H
#define CPP_RUNTIME_RUNTIMERESPONSE_H

#include <string>
#include <json/value.h>
#include "RuntimeRequest.h"

namespace runtime
{
    struct RuntimeResponse
    {
        std::string data;
        int statusCode{200};

        RuntimeResponse &json(const Json::Value &json, int code = 200)
        {
            data = json.toStyledString();
            statusCode = code;
            return *this;
        }

        RuntimeResponse &send(const std::string &string, int code = 200)
        {
            data = string;
            statusCode = code;
            return *this;
        }
    };
}

#endif //CPP_RUNTIME_RUNTIMERESPONSE_H
