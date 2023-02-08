#include <iostream>
#include <string>
#include <curl/curl.h>

static size_t WriteCallback(void *contents, size_t size, size_t nmemb, void *userp)
{
    ((std::string *) userp)->append((char *) contents, size * nmemb);
    return size * nmemb;
}

static RuntimeOutput main(RuntimeContext context)
{
    auto req = context.req;
    auto res = context.res;

    std::string headerData = req.headers["x-test-header"].asString();

    // TODO: This fails because it's std::any
    Json::Value payload = req.body;
    std::string id = payload["id"].asString();

    Json::CharReaderBuilder builder;
    Json::CharReader *reader = builder.newCharReader();

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

    return res.json(response);
}