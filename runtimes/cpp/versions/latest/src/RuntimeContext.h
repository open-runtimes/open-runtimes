#ifndef CPP_RUNTIME_RUNTIMECONTEXT_H
#define CPP_RUNTIME_RUNTIMECONTEXT_H

#include <string>
#include "RuntimeRequest.h"
#include "RuntimeResponse.h"
#include "RuntimeLogger.h"

namespace runtime
{
    class RuntimeContext
    {
        public:
            RuntimeRequest req;
            RuntimeResponse res;
            std::shared_ptr<runtime::RuntimeLogger> logger;

            void log(const std::string &message)
            {
                logger->write(message, "log");
            }

            void error(const std::string &message)
            {
                logger->write(message, "error");
            }
    };
}

#endif //CPP_RUNTIME_RUNTIMECONTEXT_H
