#ifndef CPP_RUNTIME_RUNTIMECONTEXT_H
#define CPP_RUNTIME_RUNTIMECONTEXT_H

#include "RuntimeLogger.h"
#include "RuntimeRequest.h"
#include "RuntimeResponse.h"
#include <string>

namespace runtime {
class RuntimeContext {
public:
  RuntimeRequest req;
  RuntimeResponse res;
  std::shared_ptr<runtime::RuntimeLogger> logger;

  void log(const std::string &message) { logger->write(message, "log"); }

  void error(const std::string &message) { logger->write(message, "error"); }
};
} // namespace runtime

#endif // CPP_RUNTIME_RUNTIMECONTEXT_H
