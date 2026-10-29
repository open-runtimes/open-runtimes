#ifndef CPP_RUNTIME_RUNTIMEOUTPUT_H
#define CPP_RUNTIME_RUNTIMEOUTPUT_H

#include <functional>
#include <json/value.h>
#include <optional>
#include <string>

namespace runtime {
struct RuntimeOutput {
  std::vector<std::byte> body;
  int statusCode;
  Json::Value headers;
  std::function<std::optional<std::string>()> stream;
};
} // namespace runtime
#endif // CPP_RUNTIME_RUNTIMEOUTPUT_H
