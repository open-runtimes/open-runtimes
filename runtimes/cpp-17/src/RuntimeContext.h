#ifndef CPP_RUNTIME_RUNTIMECONTEXT_H
#define CPP_RUNTIME_RUNTIMECONTEXT_H

#include <vector>
#include <string>
#include <json/value.h>
#include "RuntimeRequest.h"
#include "RuntimeResponse.h"

namespace runtime
{
    class RuntimeContext
    {
        public:
            RuntimeRequest req;
            RuntimeResponse res;

            std::vector<std::string> logs = {};
            std::vector<std::string> errors = {};

            // TODO: std::any, and check json type
            void log(std::any message)
            {
                std::string msg = std::any_cast<std::string>(message);
                logs.push_back(msg);
            }

            void error(std::any message)
            {
                std::string msg = std::any_cast<std::string>(message);
                logs.push_back(msg);
            }
    };
}

#endif //CPP_RUNTIME_RUNTIMECONTEXT_H
