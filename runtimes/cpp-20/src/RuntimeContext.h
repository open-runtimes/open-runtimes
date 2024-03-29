#ifndef CPP_RUNTIME_RUNTIMECONTEXT_H
#define CPP_RUNTIME_RUNTIMECONTEXT_H

#include <vector>
#include <string>
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

            void log(const std::string &message)
            {
                logs.push_back(message);
            }

            void error(const std::string &message)
            {
                errors.push_back(message);
            }
    };
}

#endif //CPP_RUNTIME_RUNTIMECONTEXT_H
