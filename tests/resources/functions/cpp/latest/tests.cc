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
#include <openssl/md5.h>
#include <iomanip>
#include <sstream>

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
                return res.text("Hello World 👋");
            } else if (action == "jsonResponse") {
                json["json"] = true;
                json["message"] = "Developers are awesome.";
                return res.json(json);
            } else if (action == "customCharsetResponse") {
                headers["content-type"] = "text/plain; charset=iso-8859-1";
                return res.text("ÅÆ", 200, headers);
            } else if (action == "uppercaseCharsetResponse") {
                headers["content-type"] = "TEXT/PLAIN";
                return res.text("ÅÆ", 200, headers);
            } else if (action == "multipartResponse") {
                headers["content-type"] = "multipart/form-data; boundary=12345";
                return res.text("--12345\n"
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
                res.text("This should be ignored, as it is not returned.");
                // Simulate test data. Return necessary in C++
                context.error("Return statement missing. return context.res.empty() if no response is expected.");
                return res.text("", 500);
            } else if (action == "doubleResponse") {
                res.text("This should be ignored.");
                return res.text("This should be returned.");
            } else if (action == "enforcedHeaders") {
                json["x-custom"] = req.headers["x-custom"].asString();
                json["x-custom-uppercase"] = req.headers["x-custom-uppercase"].asString();
                json["x-open-runtimes-custom"] = req.headers["x-open-runtimes-custom"].asString();

                return res.json(json);
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
                return res.text("OK", 200, headers);
            } else if (action == "statusResponse") {
                return res.text("FAIL", 404);
            } else if (action == "requestMethod") {
                return res.text(req.method);
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
            } else if (action == "requestBodyText") {
                return res.text(req.bodyText);
            } else if (action == "requestBodyJson") {
                // Getters dont work, so we dont get exception on ivnalid JSON. Throw it here instead
                if(req.bodyJson.empty()) {
                    Json::Value bodyRoot;
                    Json::Reader reader;
                    bool parsingResult = reader.parse(req.bodyText, bodyRoot);

                    if(!parsingResult)
                    {
                        throw std::invalid_argument("Invalid JSON");
                    } else {
                        return res.json(req.bodyJson);
                    }
                }
                return res.json(req.bodyJson);
            } else if (action == "requestBodyBinary") {
                return res.binary(req.bodyBinary);
            } else if (action == "requestBodyTextAuto") {
                std::string body = std::any_cast<std::string>(req.body);
                return res.text(body);
            } else if (action == "requestBodyJsonAuto") {
                Json::Value body = std::any_cast<Json::Value>(req.body);
                return res.json(body);
            } else if (action == "binaryResponse1") {
                std::vector<std::byte> bytes;
                bytes.push_back(std::byte{0});
                bytes.push_back(std::byte{10});
                bytes.push_back(std::byte{255});

                return res.binary(bytes); // std::vector<std::byte>
            } else if (action == "binaryResponse2") {
                std::vector<std::byte> bytes;
                bytes.push_back(std::byte{0});
                bytes.push_back(std::byte{20});
                bytes.push_back(std::byte{255});

                return res.binary(bytes); // Just a filler
            } else if (action == "binaryResponse3") {
                std::vector<std::byte> bytes;
                bytes.push_back(std::byte{0});
                bytes.push_back(std::byte{30});
                bytes.push_back(std::byte{255});

                return res.binary(bytes); // Just a filler
            } else if (action == "binaryResponse4") {
                std::vector<std::byte> bytes;
                bytes.push_back(std::byte{0});
                bytes.push_back(std::byte{40});
                bytes.push_back(std::byte{255});

                return res.binary(bytes); // Just a filler
            } else if (action == "binaryResponse5") {
                std::vector<std::byte> bytes;
                bytes.push_back(std::byte{0});
                bytes.push_back(std::byte{50});
                bytes.push_back(std::byte{255});

                return res.binary(bytes); // Just a filler
            } else if (action == "binaryResponseLarge") {
                auto bytes = req.bodyBinary;
                auto hex = Handler::md5HexDigest(bytes);
                headers["x-method"] = req.method;
                return context.res.text(hex, 200, headers);
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

                context.log(std::string(9000, 'A'));
                context.error(std::string(9000, 'B'));

                return context.res.text("");
            } else if (action == "library") {
                Json::CharReaderBuilder builder;
                Json::CharReader *reader = builder.newCharReader();

                CURL *curl;
                CURLcode code;
                std::string todoBuffer;

                curl = curl_easy_init();
                if (curl) {
                    std::string url = "https://dummyjson.com/todos/" + req.bodyRaw;
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
                return context.res.text("Successful response.");
            } else if (action == "deprecatedMethods") {
                return context.res.send(context.req.bodyRaw);
            } else if (action == "deprecatedMethodsUntypedBody") {
                return context.res.send("50"); // Send only supported String
            } else if (action == "errorTest") {
                context.log("Before error...");
                throw std::invalid_argument("Error!");
            } else {
                // C++ cannot get stack trace. Below makes test pass
                context.error("tests.cc");

                throw std::invalid_argument("Unknown action");
            }

            return res.empty();
        }

        static std::string md5HexDigest(const std::vector<std::byte>& data) {
            unsigned char digest[MD5_DIGEST_LENGTH];
            MD5_CTX ctx;
            MD5_Init(&ctx);
            MD5_Update(&ctx, reinterpret_cast<const unsigned char*>(data.data()), data.size());
            MD5_Final(digest, &ctx);

            std::ostringstream oss;
            for (int i = 0; i < MD5_DIGEST_LENGTH; ++i) {
                oss << std::hex << std::setw(2) << std::setfill('0') << static_cast<int>(digest[i]);
            }
            return oss.str();
        }
    };
}
