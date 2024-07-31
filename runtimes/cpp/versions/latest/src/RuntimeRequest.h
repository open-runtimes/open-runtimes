#ifndef CPP_RUNTIME_RUNTIMEREQUEST_H
#define CPP_RUNTIME_RUNTIMEREQUEST_H

#include <drogon/HttpRequest.h>
#include <json/value.h>
#include <string>

namespace runtime {
struct RuntimeRequest {
  std::string method;
  std::string scheme;
  std::string host;
  int port;
  std::string path;
  Json::Value query;
  std::string queryString;
  Json::Value headers;
  std::any body;
  std::vector<std::byte> bodyBinary;
  std::string bodyRaw;
  std::string bodyText;
  Json::Value bodyJson;
  std::string url;
};
} // namespace runtime
#endif // CPP_RUNTIME_RUNTIMEREQUEST_H
