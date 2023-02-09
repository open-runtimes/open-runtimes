#include <stdexcept>
#include <iostream>
#include <any>
#include <string>
#include <curl/curl.h>

static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp)
{
    ((std::string *) userp)->append((char *) contents, size * nmemb);
    return size * nmemb;
}

static RuntimeOutput main(RuntimeContext context) {
    auto req = context.req;
    auto res = context.res;

    Json::Value json;

    auto action = req.headers["x-action"].asString();

    if(action == "plaintextResponse") {
        return res.send("Hello World 👋");
    } else if(action == "jsonResponse") {
        json["json"] = true;
        json["message"] = "Developers are awesome.";
        return res.json(json);
    } else if(action == "redirectResponse") {
        return res.redirect("https://github.com/");
    } else if(action == "emptyResponse") {
        return res.empty();
    } else if(action == "noResponse") {
        res.send("This should be ignored, as it is not returned.");
        // Simulate test data. Return nessessary in C++
        context.error("Return statement missing. return context.res.empty() if no response is expected.");
        return res.send("", 500);
    } else if(action == "doubleResponse") {
        res.send("This should be ignored.");
        return res.send("This should be returned.");
    } else if(action == "headersResponse") {
        auto secondHeader = req.headers["x-open-runtimes-custom-in-header"].asString();
        if(secondHeader.empty()) {
            secondHeader = "missing";
        }

        json["first-header"] = "first-value";
        json["second-header"] = secondHeader;
        json["x-open-runtimes-custom-out-header"] = "third-value";
        return res.send("OK", 200, json);
    } else if(action == "statusResponse") {
        return res.send("FAIL", 404);
    } else if(action == "requestMethod") {
        return res.send(req.method);
    } else if(action == "requestUrl") {
        return res.send(req.url);
    } else if(action == "requestHeaders") {
        return res.json(req.headers);
    } else if(action == "requestBodyPlaintext") {
        std::string body = std::any_cast<std::string>(req.body);
        return res.send(body);
    } else if(action == "requestBodyJson") {
        auto isJson = false;

        try {
            Json::Value body = std::any_cast<Json::Value>(req.body);
            isJson = true;
        } catch(const std::exception& e) {
            isJson = false;
        }

        Json::String key1 = "";
        Json::String key2 = "";

        if(isJson == true) {
            Json::Value body = std::any_cast<Json::Value>(req.body);
            key1 = body["key1"].asString();
            key2 = body["key2"].asString();

            if(key1.empty()) {
                key1 = "Missing key";
            }

            if(key2.empty()) {
                key2 = "Missing key";
            }
        } else {
            key1 = "Missing key";
            key2 = "Missing key";
        }

        json["key1"] = key1;
        json["key2"] = key2;
        json["raw"] = req.bodyString;
        return res.json(json);
    } else if(action == "envVars") {
        auto custonEnvVar = std::getenv("CUSTOM_ENV_VAR");
        auto notDefinedVar = std::getenv("NOT_DEFINED_VAR");

        json["var"] = custonEnvVar;
        json["emptyVar"] = notDefinedVar;
        return res.json(json);
    } else if(action == "logs") {
            std::cout << "Native log";
            context.log("Debug log");
            context.error("Error log");
            
            context.log(42);
            context.log(4.2);
            context.log(true);

            return context.res.send("");
    } else if(action == "library") {

    } else {
        throw std::invalid_argument("Unkonwn action");
    }

    return res.empty();
}