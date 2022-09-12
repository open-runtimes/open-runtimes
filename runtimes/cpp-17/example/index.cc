#include <iostream>
#include <string>
#include <curl/curl.h>

static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp)
{
    ((std::string *) userp)->append((char *) contents, size * nmemb);
    return size * nmemb;
}

static RuntimeResponse &main(const RuntimeRequest &req, RuntimeResponse &res)
{
    std::string headerData = req.headers["x-test-header"];
    std::string variableData = req.variables["test-variable"];
    std::string id;

    Json::CharReaderBuilder builder;
    Json::CharReader *reader = builder.newCharReader();
    Json::Value payload;

    bool parsingSuccessful = reader->parse(
        req.payload.c_str(),
        req.payload.c_str() + req.payload.size(),
        &payload,
        nullptr
    );

    if (parsingSuccessful)
    {
        id = payload["id"].asString();
    }
    if (id.empty())
    {
        id = "1";
    }

    CURL *curl;
    CURLcode curlRes;
    std::string todoBuffer;

    curl = curl_easy_init();
    if (curl)
    {
        std::string url = "https://jsonplaceholder.typicode.com/todos/" + id;
        curl_easy_setopt(curl, CURLOPT_URL, url.c_str());
        curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteCallback);
        curl_easy_setopt(curl, CURLOPT_WRITEDATA, &todoBuffer);
        curlRes = curl_easy_perform(curl);
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
    response["message"] = "Hello Open Runtimes 👋";
    response["todo"] = todo;
    response["header"] = headerData;
    response["variable"] = variableData;

    return res->json(response);
}