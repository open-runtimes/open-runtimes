#include <iostream>
#include <string>

static RuntimeOutput main(const RuntimeContext context)
{
    auto req = context.req;
    auto res = context.res;

    return res.send(req.rawBody);

    /*
    Json::Value response;
    response["message"] = "Hello Open Runtimes 👋";
    response["todo"] = todo;

    auto res = context.res;
    return res.json(response);
    */
}