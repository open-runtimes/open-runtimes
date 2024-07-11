#ifndef CPP_RUNTIME_RUNTIMEOUTPUT_H
#define CPP_RUNTIME_RUNTIMEOUTPUT_H

#include <string>
#include <json/value.h>

namespace runtime
{
    struct RuntimeOutput
    {
        std::vector<std::byte> body;
        int statusCode;
        Json::Value headers;
    };
}
#endif //CPP_RUNTIME_RUNTIMEOUTPUT_H
