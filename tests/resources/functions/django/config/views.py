import base64
import hashlib
import json
import os
import sys
import time
import traceback
import urllib.request

from django.core.exceptions import RequestDataTooBig
from django.http import HttpResponse, StreamingHttpResponse

# Answers the shared Serverless test actions (`x-action` header) the way a Django
# app would, so the Django helper runs the same suite as the Flask one.


def text(body, status=200, content_type="text/plain; charset=utf-8"):
    return HttpResponse(body, status=status, content_type=content_type)


def json_response(value, status=200):
    return text(
        json.dumps(value, separators=(",", ":"), ensure_ascii=False),
        status,
        "application/json; charset=utf-8",
    )


def url_of(request):
    scheme = request.headers.get("x-forwarded-proto", "http")
    default_port = "443" if scheme == "https" else "80"
    host, _, host_port = request.headers.get("host", "").partition(":")
    port = int(host_port or default_port)
    path = request.META.get("PATH_INFO", "")
    query_string = request.META.get("QUERY_STRING", "")
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
    return StreamingHttpResponse(chunks, status=status, headers=headers)


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


def logs(request):
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


def library(request):
    todo_id = request.body.decode()
    # urllib's default User-Agent (Python-urllib) is refused by many sites
    library_request = urllib.request.Request(
        f"https://dummyjson.com/todos/{todo_id}",
        headers={"User-Agent": "open-runtimes-tests"},
    )
    with urllib.request.urlopen(library_request) as response:
        todo = json.loads(response.read())
    return json_response({"todo": todo})


def timeout(request):
    print("Timeout start.")
    time.sleep(3)
    print("Timeout end.")
    return text("Successful response.")


def stream_timeout_response(request):
    time.sleep(1)
    return stream(stream_timeout())


def spread_operator_logs(request):
    engine = "open-runtimes"
    print("engine:", engine)
    print("engine:", engine, file=sys.stderr)
    return text("OK")


def set_cookie(request):
    # Django writes cookies itself, as `cookie=value; Path=/`
    response = text("OK")
    response.set_cookie("cookie", "value", path="/")
    response.set_cookie("cookie2", "value2", path="/")
    response["some-header"] = "some-value"
    return response


def error_test(request):
    print("Before error...")
    raise Exception("Error!")


PNG = "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAAXNSR0IB2cksfwAAAAlwSFlzAAAsSwAALEsBpT2WqQAAAMlQTFRFAAAA/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu+zZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/Tdt/TZv/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZv/TZv/TZu/TZu/TZu/TZu/TZu/TZu/Tdv/TZu/TZuuSxTMwAAAEN0Uk5TABN71PrYkxIu5P/jNyDf60XK3vkOWv11JiVsYazyawE8zInhtgd8bXTitQaw8WcBHMbPXP7pciMWIDLd1yIx5hWA/BXEE2wAAACqSURBVHicXY7PCwFxEMXfkxVKkVDbHrb2YMVBOZM/Xzk52AO1tYpNSdRGfhTzne8qvMM085mZ1yMAiky5wQxAWUZtmSmo8QkrhyeD63J5rxZ5ldvCAWzJoSNfPL+Axhb0jmiSCeCLE2MwSOFyraYdre0M3ko9u5c/EG4U9BL5Xpp2ECsQ04BcAEObjyNG6NvseV5/D1RC5mkl+niX4kuymXD+CzDlozT7gDdmIiQgwIp6VQAAAABJRU5ErkJggg=="

ACTIONS = {
    "plaintextResponse": lambda request: text("Hello World 👋"),
    "jsonResponse": lambda request: json_response(
        {"json": True, "message": "Developers are awesome."}
    ),
    "customCharsetResponse": lambda request: text(
        "ÅÆ".encode("latin-1"), content_type="text/plain; charset=iso-8859-1"
    ),
    "uppercaseCharsetResponse": lambda request: text("ÅÆ"),
    "multipartResponse": lambda request: text(
        """--12345
Content-Disposition: form-data; name="partOne"

Why just have one part?
--12345
Content-Disposition: form-data; name="partTwo"

When you can have two!
--12345--""",
        content_type="multipart/form-data; boundary=12345",
    ),
    "redirectResponse": lambda request: HttpResponse(
        status=301, headers={"location": "https://github.com/"}
    ),
    "emptyResponse": lambda request: HttpResponse(status=204),
    "headersResponse": lambda request: HttpResponse(
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
    "statusResponse": lambda request: text("FAIL", 404),
    "requestMethod": lambda request: text(request.method),
    "requestUrl": lambda request: json_response(url_of(request)),
    "requestHeaders": lambda request: json_response(
        {name.lower(): value for name, value in request.headers.items()}
    ),
    "requestBodyText": lambda request: text(request.body.decode()),
    "requestBodyTextAuto": lambda request: text(request.body.decode()),
    "requestBodyJson": lambda request: json_response(json.loads(request.body)),
    "requestBodyJsonAuto": lambda request: json_response(
        json.loads(request.body or b"{}")
    ),
    "requestBodyBinary": lambda request: HttpResponse(
        request.body, content_type="application/octet-stream"
    ),
    "binaryResponse1": lambda request: HttpResponse(bytes([0, 10, 255])),
    "binaryResponse2": lambda request: HttpResponse(bytes([0, 20, 255])),
    "binaryResponse3": lambda request: HttpResponse(bytes([0, 30, 255])),
    "binaryResponse4": lambda request: HttpResponse(bytes([0, 40, 255])),
    "binaryResponse5": lambda request: HttpResponse(bytes([0, 50, 255])),
    "binaryResponseLarge": lambda request: HttpResponse(
        hashlib.md5(request.body).hexdigest(),
        content_type="text/plain",
        headers={"x-method": request.method},
    ),
    "streamResponse": lambda request: stream(
        stream_response(), 201, {"x-stream": "yes"}
    ),
    "streamException": lambda request: stream(stream_exception()),
    "streamTimeout": stream_timeout_response,
    "streamEmpty": lambda request: stream(iter([])),
    "streamSingle": lambda request: stream(iter(["only\n"])),
    "streamLong": lambda request: stream(f"chunk-{i}\n" for i in range(10000)),
    "streamBinary": lambda request: stream(
        iter([bytes(range(128)), bytes(range(128, 256))])
    ),
    "streamJson": lambda request: stream(
        stream_json(), headers={"content-type": "application/x-ndjson"}
    ),
    "streamErrorStart": lambda request: stream(stream_error_start()),
    "envVars": lambda request: json_response(
        {
            "var": os.environ.get("CUSTOM_ENV_VAR"),
            "emptyVar": os.environ.get("NOT_DEFINED_VAR"),
        }
    ),
    "logs": logs,
    "library": library,
    "timeout": timeout,
    "deprecatedMethods": lambda request: text(request.body),
    "deprecatedMethodsUntypedBody": lambda request: text("50"),
    "deprecatedMethodsBytesBody": lambda request: HttpResponse(
        base64.b64decode(PNG), content_type="image/png"
    ),
    "enforcedHeaders": lambda request: json_response(
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


def user(request, id):
    return json_response({"id": id, "sort": request.GET.get("sort")})


def action(request):
    handler = ACTIONS.get(request.headers.get("x-action"))
    if handler is None:
        raise Exception("Unknown action")

    try:
        return handler(request)
    except RequestDataTooBig:
        # Django answers 400 for a body over DATA_UPLOAD_MAX_MEMORY_SIZE; like the
        # other test apps, this one answers 413
        print(traceback.format_exc(), file=sys.stderr)
        return HttpResponse(status=413)


# Errors go to the execution's error logs; the client gets an empty 500
def server_error(request):
    print(traceback.format_exc(), file=sys.stderr)
    return HttpResponse(status=500)
