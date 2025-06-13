from hashlib import md5
import requests
import os
import asyncio
import base64


async def main(context):
    action = context.req.headers.get("x-action", None)

    if action == "plaintextResponse":
        return context.res.text("Hello World 👋")
    elif action == "jsonResponse":
        return context.res.json({"json": True, "message": "Developers are awesome."})
    elif action == "customCharsetResponse":
        return context.res.text(
            "ÅÆ", 200, {"content-type": "text/plain; charset=iso-8859-1"}
        )
    elif action == "uppercaseCharsetResponse":
        return context.res.text("ÅÆ", 200, {"content-type": "TEXT/PLAIN"})
    elif action == "multipartResponse":
        return context.res.text(
            """--12345
Content-Disposition: form-data; name=\"partOne\"

Why just have one part?
--12345
Content-Disposition: form-data; name=\"partTwo\"

When you can have two!
--12345--""",
            200,
            {"content-type": "multipart/form-data; boundary=12345"},
        )
    elif action == "redirectResponse":
        return context.res.redirect("https://github.com/")
    elif action == "emptyResponse":
        return context.res.empty()
    elif action == "noResponse":
        context.res.text("This should be ignored, as it is not returned.")
    elif action == "doubleResponse":
        context.res.text("This should be ignored.")
        return context.res.text("This should be returned.")
    elif action == "enforcedHeaders":
        return context.res.json(
            {
                "x-custom": context.req.headers["x-custom"],
                "x-custom-uppercase": context.req.headers["x-custom-uppercase"],
                "x-open-runtimes-custom": context.req.headers["x-open-runtimes-custom"],
            }
        )
    elif action == "headersResponse":
        return context.res.text(
            "OK",
            200,
            {
                "first-header": "first-value",
                "second-header": context.req.headers.get(
                    "x-open-runtimes-custom-in-header", "missing"
                ),
                "cookie": context.req.headers.get("cookie", "missing"),
                "x-open-runtimes-custom-out-header": "third-value",
            },
        )
    elif action == "statusResponse":
        return context.res.text("FAIL", 404)
    elif action == "requestMethod":
        return context.res.text(context.req.method)
    elif action == "requestUrl":
        return context.res.json(
            {
                "url": context.req.url,
                "port": context.req.port,
                "path": context.req.path,
                "query": context.req.query,
                "queryString": context.req.query_string,
                "scheme": context.req.scheme,
                "host": context.req.host,
            }
        )
    elif action == "requestHeaders":
        return context.res.json(context.req.headers)
    elif action == "requestBodyText":
        return context.res.text(context.req.body_text)
    elif action == "requestBodyJson":
        return context.res.json(context.req.body_json)
    elif action == "requestBodyBinary":
        return context.res.binary(context.req.body_binary)
    elif action == "requestBodyTextAuto":
        return context.res.text(context.req.body)
    elif action == "requestBodyJsonAuto":
        return context.res.json(context.req.body)
    elif action == "binaryResponse1":
        return context.res.binary(bytearray([0, 10, 255]))  # bytearray
    elif action == "binaryResponse2":
        return context.res.binary(bytes([0, 20, 255]))  # bytes
    elif action == "binaryResponse3":
        return context.res.binary(bytearray([0, 30, 255]))  # Just a filler
    elif action == "binaryResponse4":
        return context.res.binary(bytearray([0, 40, 255]))  # Just a filler
    elif action == "binaryResponse5":
        return context.res.binary(bytearray([0, 50, 255]))  # Just a filler
    elif action == "binaryResponseLarge":
        bytes_body = context.req.body_binary
        hex = md5(bytes_body).hexdigest()
        return context.res.text(hex, 200, {"x-method": context.req.method})
    elif action == "envVars":
        return context.res.json(
            {
                "var": os.environ.get("CUSTOM_ENV_VAR", None),
                "emptyVar": os.environ.get("NOT_DEFINED_VAR", None),
            }
        )
    elif action == "logs":
        print("Native log")
        context.log("Debug log")
        context.error("Error log")

        context.log("Log+With+Plus+Symbol")

        context.log(42)
        context.log(4.2)
        context.log(True)

        context.log({"objectKey": "objectValue"})
        context.log(["arrayValue"])

        return context.res.text("")
    elif action == "library":
        todo = (
            requests.get(
                "https://dummyjson.com/todos/" + context.req.body_raw
            )
        ).json()
        return context.res.json({"todo": todo})
    elif action == "timeout":
        context.log("Timeout start.")
        await asyncio.sleep(3)
        context.log("Timeout end.")
        return context.res.text("Successful response.")
    elif action == "deprecatedMethods":
        return context.res.send(context.req.body_raw)
    elif action == "deprecatedMethodsUntypedBody":
        return context.res.send(
            "50"
        )  # Integer never worked, it used to give TypeError: 'int' object is not iterable
    elif action == "deprecatedMethodsBytesBody":
        # Buffer from base64. It's content as MD5 is 2a8fdeea08e939e9a7c05653544a1374
        image = base64.b64decode(
            "iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAMAAAAoLQ9TAAAAAXNSR0IB2cksfwAAAAlwSFlzAAAsSwAALEsBpT2WqQAAAMlQTFRFAAAA/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu+zZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZu/Tdt/TZv/TZu/TZu/TZu/TZu/TZu/TZu/TZu/TZv/TZv/TZu/TZu/TZu/TZu/TZu/TZu/Tdv/TZu/TZuuSxTMwAAAEN0Uk5TABN71PrYkxIu5P/jNyDf60XK3vkOWv11JiVsYazyawE8zInhtgd8bXTitQaw8WcBHMbPXP7pciMWIDLd1yIx5hWA/BXEE2wAAACqSURBVHicXY7PCwFxEMXfkxVKkVDbHrb2YMVBOZM/Xzk52AO1tYpNSdRGfhTzne8qvMM085mZ1yMAiky5wQxAWUZtmSmo8QkrhyeD63J5rxZ5ldvCAWzJoSNfPL+Axhb0jmiSCeCLE2MwSOFyraYdre0M3ko9u5c/EG4U9BL5Xpp2ECsQ04BcAEObjyNG6NvseV5/D1RC5mkl+niX4kuymXD+CzDlozT7gDdmIiQgwIp6VQAAAABJRU5ErkJggg=="
        )
        return context.res.send(image, 200, {"content-type": "image/png"})
    elif action == "spreadOperatorLogs":
        engine = "open-runtimes"
        context.log("engine:", engine)
        context.error("engine:", engine)
        return context.res.text("OK")
    else:
        raise Exception("Unknown action")
