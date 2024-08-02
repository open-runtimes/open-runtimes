#ifndef CPP_RUNTIME_RUNTIMEOUTPUT_H
#define CPP_RUNTIME_RUNTIMEOUTPUT_H

#include <json/value.h>
#include <string>

namespace runtime {
struct RuntimeOutput {
  std::vector<std::byte> body;
  int statusCode;
  Json::Value headers;
};
} // namespace runtime
#endif // CPP_RUNTIME_RUNTIMEOUTPUT_H
