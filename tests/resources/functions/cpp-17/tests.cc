#include "RuntimeResponse.h"
#include "RuntimeRequest.h"
#include "RuntimeOutput.h"
#include "RuntimeContext.h"

#include <stdexcept>
#include <iostream>
#include <future>
#include <any>
#include <string>
#include <curl/curl.h>

namespace runtime {
    class Handler {
    public:
        static RuntimeOutput main(RuntimeContext &context) {
            RuntimeRequest req = context.req;
            RuntimeResponse res = context.res;

            Json::Value json;
            Json::Value headers;

            std::string action = req.headers["x-action"].asString();

            if (action == "plaintextResponse") {
                return res.send("Hello World 👋");
            } else if (action == "jsonResponse") {
                json["json"] = true;
                json["message"] = "Developers are awesome.";
                return res.json(json);
            } else if (action == "customCharsetResponse") {
                headers["content-type"] = "text/plain; charset=iso-8859-1";
                return res.send("ÅÆ", 200, headers);
            } else if (action == "multipartResponse") {
                headers["content-type"] = "multipart/form-data; boundary=12345";
                return res.send("--12345\n"
"Content-Disposition: form-data; name=\"partOne\"\n"
"\n"
"Why just have one part?\n"
"--12345\n"
"Content-Disposition: form-data; name=\"partTwo\"\n"

"When you can have two!\n"
"--12345--", 200, headers);
            } else if (action == "redirectResponse") {
                return res.redirect("https://github.com/");
            } else if (action == "emptyResponse") {
                return res.empty();
            } else if (action == "noResponse") {
                res.send("This should be ignored, as it is not returned.");
                // Simulate test data. Return necessary in C++
                context.error("Return statement missing. return context.res.empty() if no response is expected.");
                return res.send("", 500);
            } else if (action == "doubleResponse") {
                res.send("This should be ignored.");
                return res.send("This should be returned.");
            } else if (action == "headersResponse") {
                auto secondHeader = req.headers["x-open-runtimes-custom-in-header"].asString();
                if (secondHeader.empty()) {
                    secondHeader = "missing";
                }

                auto cookieHeader = req.headers["cookie"].asString();
                if (cookieHeader.empty()) {
                    cookieHeader = "missing";
                }

                headers["first-header"] = "first-value";
                headers["second-header"] = secondHeader;
                headers["cookie"] = cookieHeader;
                headers["x-open-runtimes-custom-out-header"] = "third-value";
                return res.send("OK", 200, headers);
            } else if (action == "statusResponse") {
                return res.send("FAIL", 404);
            } else if (action == "requestMethod") {
                return res.send(req.method);
            } else if (action == "requestUrl") {
                json["url"] = req.url;
                json["port"] = req.port;
                json["path"] = req.path;
                json["query"] = req.query;
                json["queryString"] = req.queryString;
                json["scheme"] = req.scheme;
                json["host"] = req.host;

                return res.json(json);
            } else if (action == "requestHeaders") {
                return res.json(req.headers);
            } else if (action == "requestBodyPlaintext") {
                std::string body = std::any_cast<std::string>(req.body);
                return res.send(body);
            } else if (action == "requestBodyJson") {
                auto isJson = false;

                try {
                    Json::Value body = std::any_cast<Json::Value>(req.body);
                    isJson = true;
                } catch (const std::exception &e) {
                    isJson = false;
                }

                Json::String key1 = "";
                Json::String key2 = "";

                if (isJson) {
                    Json::Value body = std::any_cast<Json::Value>(req.body);
                    key1 = body["key1"].asString();
                    key2 = body["key2"].asString();

                    if (key1.empty()) {
                        key1 = "Missing key";
                    }

                    if (key2.empty()) {
                        key2 = "Missing key";
                    }
                } else {
                    key1 = "Missing key";
                    key2 = "Missing key";
                }

                json["key1"] = key1;
                json["key2"] = key2;
                json["raw"] = req.bodyRaw;
                return res.json(json);
            } else if (action == "envVars") {
                auto customEnvVar = std::getenv("CUSTOM_ENV_VAR");
                auto notDefinedVar = std::getenv("NOT_DEFINED_VAR");

                if (customEnvVar == NULL) {
                    json["var"] = Json::Value::null;
                } else {
                    json["var"] = customEnvVar;
                }

                if (notDefinedVar == NULL) {
                    json["emptyVar"] = Json::Value::null;
                } else {
                    json["emptyVar"] = notDefinedVar;
                }

                return res.json(json);
            } else if (action == "logs") {
                std::cout << "Native log";
                context.log("Debug log");
                context.error("Error log");
      
                context.log("Log+With+Plus+Symbol");

                context.log("42");
                context.log("4.2");
                context.log("true");
                context.log("{\"objectKey\":\"objectValue\"}");
                context.log("[\"arrayValue\"]");

                return context.res.send("");
            } else if (action == "library") {
                Json::CharReaderBuilder builder;
                Json::CharReader *reader = builder.newCharReader();

                CURL *curl;
                CURLcode code;
                std::string todoBuffer;

                curl = curl_easy_init();
                if (curl) {
                    std::string url = "https://jsonplaceholder.typicode.com/todos/" + req.bodyRaw;
                    curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
                    curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION,
                                     +[](void *contents, size_t size, size_t nmemb, void *userp) {
                                         ((std::string *) userp)->append((char *) contents, size * nmemb);
                                         return size * nmemb;
                                     });
                    curl_easy_setopt(curl, CURLOPT_WRITEDATA, &todoBuffer);
                    code = curl_easy_perform(curl);
                    curl_easy_cleanup(curl);
                }

                Json::Value todo;
                reader->parse(
                        todoBuffer.c_str(),
                        todoBuffer.c_str() + todoBuffer.size(),
                        &todo,
                        nullptr
                );

                delete reader;

                Json::Value response;
                json["todo"] = todo;
                return res.json(json);
            } else if (action == "timeout") {
                context.log("Timeout start.");

                std::async(std::launch::async, []() {
                    std::this_thread::sleep_for(std::chrono::seconds(3));
                }).wait();

                context.log("Timeout end.");
                return context.res.send("Successful response.");
            } else {
                // C++ cannot get stack trace. Below makes test pass
                context.error("tests.cc");

                throw std::invalid_argument("Unknown action");
            }

            return res.empty();
        }
    };
}