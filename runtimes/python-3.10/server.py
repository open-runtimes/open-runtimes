from flask import Flask, request, Response as FlaskResponse
from urllib.parse import urlparse
from io import StringIO
import traceback
import pathlib
import os
import importlib
import sys
import urllib.parse
import json
import asyncio

app = Flask(__name__)

class Response:
    def send(self, body, statusCode = 200, headers = {}):
        return {
            'body': body,
            'statusCode': statusCode,
            'headers': headers,
        }

    def json(self, obj, statusCode = 200, headers = {}):
        headers['content-type'] = 'application/json'
        return self.send(json.dumps(obj, separators=(',', ':')), statusCode, headers)
    
    def empty(self):
        return self.send('', 204, {})

    def redirect(self, url, statusCode = 301, headers = {}):
        headers['location'] = url
        return self.send('', statusCode, headers)

class Request:
    body_string = None
    body = None
    headers = None
    method = None
    url = None
    path = None
    port = None
    query = None
    query_string = None
    scheme = None
    host = None

class Context:
    req = Request()
    res = Response()

    logs = []
    errors = []

    def __init__(self):
        self.logs = []
        self.errors = []
        self.req = Request()
        self.res = Response()

    def log(self, message):
        if isinstance(message, (list, dict, tuple)):
            self.logs.append(json.dumps(message, separators=(',', ':')))
        else:
            self.logs.append(str(message))

    def error(self, message):
        if isinstance(message, (list, dict, tuple)):
            self.errors.append(json.dumps(message, separators=(',', ':')))
        else:
            self.errors.append(str(message))

HTTP_METHODS = ['GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'CONNECT', 'OPTIONS', 'TRACE', 'PATCH']

@app.route('/', defaults={'u_path': ''}, methods = HTTP_METHODS)
@app.route('/<path:u_path>', methods = HTTP_METHODS)
async def handler(u_path):
    timeout = request.headers.get('x-open-runtimes-timeout', '')
    safeTimeout = None
    if (timeout):
        if not timeout.isdigit():
            return 'Header "x-open-runtimes-timeout" must be an integer.', 500
            
        safeTimeout = int(timeout)

    if (request.headers.get('x-open-runtimes-secret', '') == '' or request.headers.get('x-open-runtimes-secret', '') != os.getenv('OPEN_RUNTIMES_SECRET', '')):
        return 'Unauthorized. Provide correct "x-open-runtimes-secret" header.', 500

    context = Context()

    context.req.body_string = request.get_data(as_text=True)
    context.req.body = context.req.body_string
    context.req.method = request.method
    context.req.headers = {}

    context.req.path = request.path
    context.req.scheme = request.headers.get('x-forwarded-proto', 'http')
    
    defaultPort = "443" if context.req.scheme == "https" else "80"

    url = urlparse(request.url)
    context.req.query_string = url.query or ''
    context.req.query = {}

    for param in context.req.query_string.split('&'):
        pair = param.split('=', 1)

        if pair[0]:
            context.req.query[pair[0]] = pair[1] if len(pair) > 1 else ''

    host = request.headers.get('host', '')
    if ':' in host:
        context.req.host = host.split(':')[0]
        context.req.port = int(host.split(':')[1])
    else:
        context.req.host = host
        context.req.port = int(defaultPort)

    context.req.url = context.req.scheme + '://' + context.req.host

    if(context.req.port != int(defaultPort)):
        context.req.url += ':' + str(context.req.port)

    context.req.url += context.req.path

    if(context.req.query_string):
        context.req.url += '?' + context.req.query_string

    contentType = request.headers.get('content-type', 'text/plain')
    if 'application/json' in contentType:
        if not context.req.body_string:
            context.req.body = {}
        else:
            context.req.body = request.get_json(force=True, silent=False)

    headers = dict(request.headers)
    for key in headers.keys():
        if not key.lower().startswith('x-open-runtimes-'):
            context.req.headers[key.lower()] = headers[key]

    sys.stdout = sys.stderr = customstd = StringIO()

    output = None

    async def execute(context):
        userPath = os.getenv('OPEN_RUNTIMES_ENTRYPOINT')
        if userPath.endswith('.py'):
            size = len(userPath)
            userPath = userPath[:size - 3]
        userPath = userPath.replace("/", ".")
        userModule = importlib.import_module("userlib." + userPath)

        if userModule is None:
            raise Exception('Code file not found.')

        if asyncio.iscoroutinefunction(userModule.main):
            output = await userModule.main(context)
        else:
            output = userModule.main(context)

        return output

    try:
        if(safeTimeout is not None):
            try:
                output = await asyncio.wait_for(execute(context), timeout=safeTimeout)
            except asyncio.TimeoutError:
                context.error('Execution timed out.')
                output = context.res.send('', 500, {})
        else:
            output = await execute(context)
    except Exception as e:
        context.error(''.join(traceback.TracebackException.from_exception(e).format()))
        output = context.res.send('', 500, {})
    finally:
        sys.stdout = sys.__stdout__
        sys.stderr = sys.__stderr__

    if output is None:
        context.error('Return statement missing. return context.res.empty() if no response is expected.')
        output = context.res.send('', 500, {})

    output['body'] = output.get('body', '')
    output['statusCode'] = output.get('statusCode', 200)
    output['headers'] = output.get('headers', {})

    resp = FlaskResponse(output['body'], output['statusCode'])

    for key in output['headers'].keys():
        if not key.lower().startswith('x-open-runtimes-'):
            resp.headers[key.lower()] = output['headers'][key]

    if customstd.getvalue():
        context.log('Unsupported log detected. Use context.log() or context.error() for logging.')

    resp.headers['x-open-runtimes-logs'] = urllib.parse.quote('\n'.join(context.logs))
    resp.headers['x-open-runtimes-errors'] = urllib.parse.quote('\n'.join(context.errors))

    return resp

if __name__ == "__main__":
    from waitress import serve
    serve(app, host="0.0.0.0", port=3000)