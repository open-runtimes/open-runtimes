import base64
import hashlib
import json
import os
import sys
import time
import traceback
import urllib.request

from flask import Flask, Response, request

# Answers the shared Serverless test actions (`x-action` header) the way a Flask
# app would, so the Flask helper runs the same suite as the Express one.

app = Flask(__name__)
app.config["MAX_CONTENT_LENGTH"] = 20 * 1024 * 1024


def text(body, status=200, content_type="text/plain; charset=utf-8"):
    return Response(body, status=status, content_type=content_type)


def json_response(value, status=200):
    return text(
        json.dumps(value, separators=(",", ":"), ensure_ascii=False),
        status,
        "application/json; charset=utf-8",
    )


def url_of():
    scheme = request.headers.get("x-forwarded-proto", "http")
    default_port = "443" if scheme == "https" else "80"
    host, _, host_port = request.headers.get("host", "").partition(":")
    port = int(host_port or default_port)
    path = request.environ.get("PATH_INFO", "")
    query_string = request.environ.get("QUERY_STRING", "")
    query = {}
    for param in query_string.split("&"):
        key, _, value = param.partition("=")
        if key:
            query[key] = value

    shown_port = "" if str(port) == default_port else f":{port}"
    shown_query = f"?{query_string}" if query_string else ""
    return {
        "url": f"{scheme}://{host}{shown_port}{path}{shown_query}",
        "port": port,
        "path": path,
        "query": query,
        "queryString": query_string,
        "scheme": scheme,
        "host": host,
    }


def stream(chunks, status=200, headers=None):
    return Response(chunks(), status=status, headers=headers or {})


def stream_response():
    print("Stream log.")
    yield "first\n"
    time.sleep(1)
    yield "second\n"
    time.sleep(1)
    yield "third\n"


def stream_exception():
    yield "first\n"
    raise Exception("Stream failed")


def stream_timeout():
    yield "first\n"
    time.sleep(5)
    yield "second\n"


def stream_json():
    for n in range(1, 4):
        if n > 1:
            time.sleep(0.1)
        yield json.dumps({"n": n}, separators=(",", ":")) + "\n"


def stream_error_start():
    raise Exception("Stream failed early")
    yield ""


def logs():
    print("Native log")
    print("Debug log")
    print("Error log", file=sys.stderr)
    print("Log+With+Plus+Symbol")
    print(42)
    print(4.2)
    print(True)
    print(json.dumps({"objectKey": "objectValue"}))
    print(json.dumps(["arrayValue"]))
    print(",".join(["A"] * 9000))
    print(",".join(["B"] * 9000), file=sys.stderr)
    return text("")


def library():
    todo_id = request.get_data(as_text=True)
    # urllib's default User-Agent (Python-urllib) is refused by many sites
    library_request = urllib.request.Request(
        f"https://dummyjson.com/todos/{todo_id}",
        headers={"User-Agent": "open-runtimes-tests"},
    )
    with urllib.request.urlopen(library_request) as response:
        todo = json.loads(response.read())
    return json_response({"todo": todo})


def timeout():
    print("Timeout start.")
    time.sleep(3)
    print("Timeout end.")
    return text("Successful response.")


def spread_operator_logs():
    engine = "open-runtimes"
    print("engine:", engine)
    print("engine:", engine, file=sys.stderr)
    return text("OK")


def set_cookie():
    response = text("OK")
    response.headers.add("set-cookie", "cookie=value; path=/")
    response.headers.add("set-cookie", "cookie2=value2; path=/")
    response.headers["some-header"] = "some-value"
    return response


def error_test():
    print("Before error...")
    raise Exception("Error!")


PNG = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAAXNSR0IB2cksfwAAAAlwSFlzAAAsSwAALEsBpT2WqQAAAMlQTFRFAAAA/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu+zZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/Tdt/TZv/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZv/TZv/TZu/TZu/TZu/TZu/TZu/TZu/Tdv/TZu/TZuuSxTMwAAAEN0Uk5TABN71PrYkxIu5P/jNyDf60XK3vkOWv11JiVsYazyawE8zInhtgd8bXTitQaw8WcBHMbPXP7pciMWIDLd1yIx5hWA/BXEE2wAAACqSURBVHicXY7PCwFxEMXfkxVKkVDbHrb2YMVBOZM/Xzk52AO1tYpNSdRGfhTzne8qvMM085mZ1yMAiky5wQxAWUZtmSmo8QkrhyeD63J5rxZ5ldvCAWzJoSNfPL+Axhb0jmiSCeCLE2MwSOFyraYdre0M3ko9u5c/EG4U9BL5Xpp2ECsQ04BcAEObjyNG6NvseV5/D1RC5mkl+niX4kuymXD+CzDlozT7gDdmIiQgwIp6VQAAAABJRU5ErkJggg=="

ACTIONS = {
    "plaintextResponse": lambda: text("Hello World 👋"),
    "jsonResponse": lambda: json_response(
        {"json": True, "message": "Developers are awesome."}
    ),
    "customCharsetResponse": lambda: text(
        "ÅÆ".encode("latin-1"), content_type="text/plain; charset=iso-8859-1"
    ),
    # Flask sets the charset of a text mimetype itself
    "uppercaseCharsetResponse": lambda: Response("ÅÆ", mimetype="text/plain"),
    "multipartResponse": lambda: text(
        """--12345
Content-Disposition: form-data; name="partOne"

Why just have one part?
--12345
Content-Disposition: form-data; name="partTwo"

When you can have two!
--12345--""",
        content_type="multipart/form-data; boundary=12345",
    ),
    "redirectResponse": lambda: Response(
        status=301, headers={"location": "https://github.com/"}
    ),
    "emptyResponse": lambda: Response(status=204),
    "headersResponse": lambda: Response(
        "OK",
        content_type="text/plain",
        headers={
            "first-header": "first-value",
            "second-header": request.headers.get(
                "x-open-runtimes-custom-in-header", "missing"
            ),
            "cookie": request.headers.get("cookie", "missing"),
            "x-open-runtimes-custom-out-header": "third-value",
        },
    ),
    "statusResponse": lambda: text("FAIL", 404),
    "requestMethod": lambda: text(request.method),
    "requestUrl": lambda: json_response(url_of()),
    "requestHeaders": lambda: json_response(
        {name.lower(): value for name, value in request.headers.items()}
    ),
    "requestBodyText": lambda: text(request.get_data(as_text=True)),
    "requestBodyTextAuto": lambda: text(request.get_data(as_text=True)),
    "requestBodyJson": lambda: json_response(json.loads(request.get_data())),
    "requestBodyJsonAuto": lambda: json_response(
        json.loads(request.get_data() or b"{}")
    ),
    "requestBodyBinary": lambda: Response(
        request.get_data(), content_type="application/octet-stream"
    ),
    "binaryResponse1": lambda: Response(bytes([0, 10, 255])),
    "binaryResponse2": lambda: Response(bytes([0, 20, 255])),
    "binaryResponse3": lambda: Response(bytes([0, 30, 255])),
    "binaryResponse4": lambda: Response(bytes([0, 40, 255])),
    "binaryResponse5": lambda: Response(bytes([0, 50, 255])),
    "binaryResponseLarge": lambda: Response(
        hashlib.md5(request.get_data()).hexdigest(),
        content_type="text/plain",
        headers={"x-method": request.method},
    ),
    "streamResponse": lambda: stream(stream_response, 201, {"x-stream": "yes"}),
    "streamException": lambda: stream(stream_exception),
    "streamTimeout": lambda: (time.sleep(1), stream(stream_timeout))[1],
    "streamEmpty": lambda: stream(lambda: iter([])),
    "streamSingle": lambda: stream(lambda: iter(["only\n"])),
    "streamLong": lambda: stream(lambda: (f"chunk-{i}\n" for i in range(10000))),
    "streamBinary": lambda: stream(
        lambda: iter([bytes(range(128)), bytes(range(128, 256))])
    ),
    "streamJson": lambda: stream(
        stream_json, headers={"content-type": "application/x-ndjson"}
    ),
    "streamErrorStart": lambda: stream(stream_error_start),
    "envVars": lambda: json_response(
        {
            "var": os.environ.get("CUSTOM_ENV_VAR"),
            "emptyVar": os.environ.get("NOT_DEFINED_VAR"),
        }
    ),
    "logs": logs,
    "library": library,
    "timeout": timeout,
    "deprecatedMethods": lambda: text(request.get_data()),
    "deprecatedMethodsUntypedBody": lambda: text("50"),
    "deprecatedMethodsBytesBody": lambda: Response(
        base64.b64decode(PNG), content_type="image/png"
    ),
    "enforcedHeaders": lambda: json_response(
        {
            "x-custom": request.headers.get("x-custom"),
            "x-custom-uppercase": request.headers.get("x-custom-uppercase"),
            "x-open-runtimes-custom": request.headers.get("x-open-runtimes-custom"),
        }
    ),
    "spreadOperatorLogs": spread_operator_logs,
    "setCookie": set_cookie,
    "setCookie2": set_cookie,
    "errorTest": error_test,
}

METHODS = ["GET", "POST", "PUT", "PATCH", "DELETE", "OPTIONS"]


# Flask routing, beyond the shared actions
@app.get("/users/<id>")
def user(id):
    return json_response({"id": id, "sort": request.args.get("sort")})


@app.route("/", defaults={"path": ""}, methods=METHODS)
@app.route("/<path:path>", methods=METHODS)
def action(path):
    handler = ACTIONS.get(request.headers.get("x-action"))
    if handler is None:
        raise Exception("Unknown action")

    return handler()


# Errors go to the execution's error logs; the client gets an empty 500 (or the
# error's status, such as 413 for a body over the limit).
@app.errorhandler(Exception)
def error(exception):
    print(traceback.format_exc(), file=sys.stderr)
    return Response(status=getattr(exception, "code", None) or 500)
