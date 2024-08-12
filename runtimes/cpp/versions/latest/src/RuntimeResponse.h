#ifndef CPP_RUNTIME_RUNTIMERESPONSE_H
#define CPP_RUNTIME_RUNTIMERESPONSE_H

#include "RuntimeOutput.h"
#include <algorithm>
#include <json/json.h>
#include <json/value.h>
#include <string>

namespace runtime {
class RuntimeResponse {
public:
  RuntimeOutput binary(const std::vector<std::byte> &bytes,
                       const int statusCode = 200,
                       const Json::Value &headers = {}) {
    RuntimeOutput output;
    output.body = bytes;
    output.statusCode = statusCode;
    output.headers = headers;
    return output;
  }

  RuntimeOutput send(const std::string &body, const int statusCode = 200,
                     const Json::Value &headers = {}) {
    return this->text(body, statusCode, headers);
  }

  RuntimeOutput text(const std::string &body, const int statusCode = 200,
                     const Json::Value &headers = {}) {
    std::vector<std::byte> bytes;
    bytes.reserve(body.size());
    std::transform(std::begin(body), std::end(body), std::back_inserter(bytes),
                   [](char c) { return std::byte(c); });

    return this->binary(bytes, statusCode, headers);
  }

  RuntimeOutput json(const Json::Value &json, const int statusCode = 200,
                     Json::Value headers = {}) {
    headers["content-type"] = "application/json";

    if (json.empty()) {
      return this->text("{}", statusCode, headers);
    }

    Json::StreamWriterBuilder wbuilder;
    wbuilder["indentation"] = "";
    std::string document = Json::writeString(wbuilder, json);
    return this->text(document, statusCode, headers);
  }

  RuntimeOutput empty() { return this->text("", 204, {}); }

  RuntimeOutput redirect(const std::string &url, const int statusCode = 301,
                         Json::Value headers = {}) {
    headers["location"] = url;
    return this->text("", statusCode, headers);
  }
};
} // namespace runtime

#endif // CPP_RUNTIME_RUNTIMERESPONSE_H
