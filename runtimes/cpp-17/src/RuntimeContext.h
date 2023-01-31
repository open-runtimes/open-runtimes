#ifndef CPP_RUNTIME_RUNTIMECONTEXT_H
#define CPP_RUNTIME_RUNTIMECONTEXT_H

#include <string>
#include <json/value.h>
#include "RuntimeRequest.h"
#include "RuntimeResponse.h"

namespace runtime
{
    struct RuntimeContext
    {
        RuntimeRequest req;
        RuntimeResponse res;
    };
}

#endif //CPP_RUNTIME_RUNTIMECONTEXT_H
