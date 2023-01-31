#ifndef CPP_RUNTIME_RUNTIMEOUTPUT_H
#define CPP_RUNTIME_RUNTIMEOUTPUT_H

#include <string>
#include <json/value.h>
#include <drogon/HttpRequest.h>

namespace runtime
{
    struct RuntimeOutput
    {
        std::string body;
        int statusCode;
        Json::Value headers;
    };
}
#endif //CPP_RUNTIME_RUNTIMEOUTPUT_H
