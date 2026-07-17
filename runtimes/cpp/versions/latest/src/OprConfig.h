#ifndef CPP_RUNTIME_OPRCONFIG_H
#define CPP_RUNTIME_OPRCONFIG_H

#include <cstdlib>
#include <json/json.h>
#include <string>

namespace runtime {
struct OprConfig {
  std::string secret;
  Json::Value headers;
  std::string env;
};

inline const OprConfig &config() {
  static const OprConfig instance = [] {
    OprConfig config;

    if (std::getenv("OPEN_RUNTIMES_SECRET") != nullptr) {
      config.secret = std::getenv("OPEN_RUNTIMES_SECRET");
    }

    if (std::getenv("OPEN_RUNTIMES_ENV") != nullptr) {
      config.env = std::getenv("OPEN_RUNTIMES_ENV");
    }

    if (std::getenv("OPEN_RUNTIMES_HEADERS") != nullptr) {
      std::string serverHeadersString(std::getenv("OPEN_RUNTIMES_HEADERS"));

      if (serverHeadersString.empty()) {
        serverHeadersString = "{}";
      }

      Json::Reader reader;
      if (!reader.parse(serverHeadersString, config.headers)) {
        config.headers = Json::Value();
      }
    }

    return config;
  }();

  return instance;
}
} // namespace runtime

#endif // CPP_RUNTIME_OPRCONFIG_H
