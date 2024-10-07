import json
from function_types import Context, Request, Response
from logger import Logger
from flask import Flask, request, Response as FlaskResponse
from urllib.parse import urlparse
from io import StringIO
import traceback
import pathlib
import os
import importlib
import urllib.parse
import asyncio
import sys

app = Flask(__name__)
app.config["MAX_CONTENT_LENGTH"] = 20 * 1024 * 1024

HTTP_METHODS = [
    "GET",
    "HEAD",
    "POST",
    "PUT",
    "DELETE",
    "CONNECT",
    "OPTIONS",
    "TRACE",
    "PATCH",
]


def action(logger, request):
    timeout = request.headers.get("x-open-runtimes-timeout", "")
    safeTimeout = None
    if timeout:
        if not timeout.isdigit() or int(timeout) == 0:
            return (
                'Header "x-open-runtimes-timeout" must be an integer greater than 0.',
                500,
            )

        safeTimeout = int(timeout)

    if os.getenv("OPEN_RUNTIMES_SECRET", "") != "" and request.headers.get(
        "x-open-runtimes-secret", ""
    ) != os.getenv("OPEN_RUNTIMES_SECRET", ""):
        return 'Unauthorized. Provide correct "x-open-runtimes-secret" header.', 500

    context = Context(logger)

    context.req.body_binary = request.get_data()
    context.req.method = request.method
    context.req.headers = {}

    context.req.path = request.path
    context.req.scheme = request.headers.get("x-forwarded-proto", "http")

    defaultPort = "443" if context.req.scheme == "https" else "80"

    url = urlparse(request.url)
    context.req.query_string = url.query or ""
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

    async def execute(context):
        userPath = os.getenv("OPEN_RUNTIMES_ENTRYPOINT")
        if userPath.endswith(".py"):
            size = len(userPath)
            userPath = userPath[: size - 3]
        userPath = userPath.replace("/", ".")
        userModule = importlib.import_module("function." + userPath)

        if userModule is None:
            raise Exception("Code file not found.")

        if asyncio.iscoroutinefunction(userModule.main):
            output = await userModule.main(context)
        else:
            output = userModule.main(context)

        return output

    try:
        if safeTimeout is not None:
            try:
                output = asyncio.run(
                    asyncio.wait_for(execute(context), timeout=safeTimeout)
                )
            except asyncio.TimeoutError:
                context.error("Execution timed out.")
                output = context.res.text("", 500, {})
        else:
            output = asyncio.run(execute(context))
    except Exception as e:
        context.error("".join(traceback.TracebackException.from_exception(e).format()))
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

    if isinstance(output["body"], bytearray):
        output["body"] = bytes(output["body"])

    resp = FlaskResponse(output["body"], output["statusCode"])

    for key in output["headers"].keys():
        if not key.lower().startswith("x-open-runtimes-"):
            resp.headers[key.lower()] = output["headers"][key]

    resp.headers["content-type"] = (
        output["headers"].get("content-type", "text/plain").lower()
    )
    if (
        not resp.headers["content-type"].startswith("multipart/")
        and not "charset=" in resp.headers["content-type"]
    ):
        resp.headers["content-type"] += "; charset=utf-8"

    resp.headers["x-open-runtimes-log-id"] = logger.id
    logger.end()

    return resp


@app.route("/", defaults={"u_path": ""}, methods=HTTP_METHODS)
@app.route("/<path:u_path>", methods=HTTP_METHODS)
def handler(u_path):
    logger = Logger(
        request.headers.get("x-open-runtimes-logging", ""),
        request.headers.get("x-open-runtimes-log-id", ""),
    )

    try:
        return action(logger, request)
    except Exception as e:
        message = "".join(traceback.TracebackException.from_exception(e).format())
        logger.write([message], Logger.TYPE_ERROR)

    resp = FlaskResponse("", 500)

    resp.headers["x-open-runtimes-log-id"] = logger.id
    logger.end()

    return resp


if __name__ == "__main__":
    from gunicorn.app.wsgiapp import run

    sys.exit(run())
