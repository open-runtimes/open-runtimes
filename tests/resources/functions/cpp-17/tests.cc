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

    } else if(action == "envVars") {

    } else if(action == "logs") {

    } else if(action == "library") {

    } else {
        throw std::invalid_argument("Unkonwn action");
    }
}