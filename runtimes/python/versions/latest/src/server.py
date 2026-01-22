import asyncio
import importlib
import json
import os
import traceback

from aiohttp import web, web_exceptions, web_request
from function_types import Context
from logger import Logger


async def action(logger, request: web_request.Request):
    timeout = request.headers.get("x-open-runtimes-timeout", "")
    safeTimeout = None
    if timeout:
        if not timeout.isdigit() or int(timeout) == 0:
            return web.Response(
                text='Header "x-open-runtimes-timeout" must be an integer greater than 0.',
                status=500,
            )

        safeTimeout = int(timeout)

    if os.getenv("OPEN_RUNTIMES_SECRET", "") != "" and request.headers.get(
        "x-open-runtimes-secret", ""
    ) != os.getenv("OPEN_RUNTIMES_SECRET", ""):
        return web.Response(
            text='Unauthorized. Provide correct "x-open-runtimes-secret" header.',
            status=500,
        )

    context = Context(logger)

    context.req.body_binary = await request.read()
    context.req.method = request.method
    context.req.headers = {}

    context.req.path = request.path
    context.req.scheme = request.headers.get("x-forwarded-proto", "http")

    defaultPort = "443" if context.req.scheme == "https" else "80"

    url = request.url
    context.req.query_string = url.query_string or ""
    context.req.query = {}

    for param in context.req.query_string.split("&"):
        pair = param.split("=", 1)

        if pair[0]:
            context.req.query[pair[0]] = pair[1] if len(pair) > 1 else ""

    host = request.headers.get("host", "")
    if ":" in host:
        context.req.host = host.split(":")[0]
        context.req.port = int(host.split(":")[1])
    else:
        context.req.host = host
        context.req.port = int(defaultPort)

    context.req.url = context.req.scheme + "://" + context.req.host

    if context.req.port != int(defaultPort):
        context.req.url += ":" + str(context.req.port)

    context.req.url += context.req.path

    if context.req.query_string:
        context.req.url += "?" + context.req.query_string

    headers = dict(request.headers)
    for key in headers.keys():
        if not key.lower().startswith("x-open-runtimes-"):
            context.req.headers[key.lower()] = headers[key]

    server_headers = os.getenv("OPEN_RUNTIMES_HEADERS", "")
    server_headers = server_headers if server_headers else "{}"
    enforced_headers = json.loads(server_headers)
    for key in enforced_headers.keys():
        context.req.headers[key.lower()] = str(enforced_headers[key])

    logger.override_native_logs()

    output = None
    userModule = None

    entrypoint = os.getenv("OPEN_RUNTIMES_ENTRYPOINT")
    entrypointFilePath = "/usr/local/server/src/function/" + entrypoint

    # Guard: Check file exists
    if not os.path.isfile(entrypointFilePath):
        context.error(
            "Failed to load entrypoint, file " + entrypoint + " does not exist."
        )
        logger.revert_native_logs()
        output = context.res.text("", 503, {})

    # Guard: Try to load module
    if output is None:
        userPath = entrypoint
        if userPath.endswith(".py"):
            userPath = userPath[:-3]
        userPath = userPath.replace("/", ".")

        try:
            userModule = importlib.import_module("function." + userPath)
            if not callable(getattr(userModule, "main", None)):
                raise AttributeError(
                    "Function signature invalid. Did you forget to export a 'main' function?"
                )
        except ModuleNotFoundError as e:
            context.error("Module not found: " + str(e.name))
            output = context.res.text("", 503, {})
        except SyntaxError as e:
            context.error(
                "Syntax error in "
                + str(e.filename)
                + ":"
                + str(e.lineno)
                + ": "
                + str(e.msg)
            )
            output = context.res.text("", 503, {})
        except AttributeError as e:
            context.error(str(e))
            output = context.res.text("", 503, {})
        except Exception as e:
            context.error(
                "Failed to load module: "
                + "".join(traceback.TracebackException.from_exception(e).format())
            )
            output = context.res.text("", 503, {})
        finally:
            if output is not None:
                logger.revert_native_logs()

    # Execute user function
    if output is None:

        async def execute(context):
            if asyncio.iscoroutinefunction(userModule.main):
                return await userModule.main(context)
            return userModule.main(context)

        try:
            if safeTimeout is not None:
                try:
                    output = await asyncio.wait_for(
                        execute(context), timeout=safeTimeout
                    )
                except asyncio.TimeoutError:
                    context.error("Execution timed out.")
                    output = context.res.text("", 500, {})
            else:
                output = await execute(context)
        except Exception as e:
            context.error(
                "".join(traceback.TracebackException.from_exception(e).format())
            )
            output = context.res.text("", 500, {})
        finally:
            logger.revert_native_logs()

    if output is None:
        context.error(
            "Return statement missing. return context.res.empty() if no response is expected."
        )
        output = context.res.text("", 500, {})

    output["body"] = output.get("body", "")
    output["statusCode"] = output.get("statusCode", 200)
    output["headers"] = output.get("headers", {})
    resp = web.Response(body=output["body"], status=output["statusCode"])

    for key in output["headers"].keys():
        if not key.lower().startswith("x-open-runtimes-"):
            resp.headers[key.lower()] = output["headers"][key]

    resp.headers["content-type"] = (
        output["headers"].get("content-type", "text/plain").lower()
    )
    if (
        not resp.headers["content-type"].startswith("multipart/")
        and "charset=" not in resp.headers["content-type"]
    ):
        resp.headers["content-type"] += "; charset=utf-8"

    resp.headers["x-open-runtimes-log-id"] = logger.id
    logger.end()

    return resp


async def handler(request) -> web.Response:
    if request.path == "/__opr/health":
        return web.Response(text="OK", status=200)

    if request.path == "/__opr/timings":
        with open("/mnt/telemetry/timings.txt", "r") as f:
            timings = f.read()
        return web.Response(
            text=timings, headers={"content-type": "text/plain; charset=utf-8"}
        )

    logger = Logger(
        request.headers.get("x-open-runtimes-logging", ""),
        request.headers.get("x-open-runtimes-log-id", ""),
    )

    try:
        return await action(logger, request)
    except web_exceptions.HTTPClientError as e:
        return e
    except Exception as e:
        message = "".join(traceback.TracebackException.from_exception(e).format())
        logger.write([message], Logger.TYPE_ERROR)

    resp = web.Response(status=500, headers={"x-open-runtimes-log-id": logger.id})
    logger.end()

    return resp


app = web.Application(client_max_size=20 * 1024 * 1024)
app.router.add_route("*", r"/{handler:.*}", handler)
