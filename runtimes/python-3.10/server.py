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

def handler(u_path):
    if (request.headers.get('x-open-runtimes-secret') != os.getenv('OPEN_RUNTIMES_SECRET')):
        return 'Unauthorized. Provide correct "x-open-runtimes-secret" header.', 500

    url = urlparse(request.url)
    path = url.path
    query = url.query
    if query:
        path += '?' + query

    contentType = request.headers.get('content-type', 'text/plain')
    if 'application/json' in contentType:
        context.req.body = request.get_json(force=True, silent=False)

    headers = dict(request.headers)
    for key in headers.keys():
        if not key.lower().startswith('x-open-runtimes-'):
            context.req.headers[key.lower()] = headers[key]

    sys.stdout = sys.stderr = customstd = StringIO()

    output = None
    try:
        userPath = os.getenv('OPEN_RUNTIMES_ENTRYPOINT')
        if userPath.endswith('.py'):
            size = len(userPath)
            userPath = userPath[:size - 3]
        userPath = userPath.replace("/", ".")
        userModule = importlib.import_module("userlib." + userPath)

        if userModule is None:
            raise Exception('Code file not found.')

        output = userModule.main(context)
    except Exception as e:
        context.error(str(e))
        # TODO: Get trace
        output = context.res.send('', 500, {})

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
        context.log('Unsupported log noticed. Use context.log() or context.error() for logging.')

    resp.headers['x-open-runtimes-logs'] = urllib.parse.quote('\n'.join(context._logs))
    resp.headers['x-open-runtimes-errors'] = urllib.parse.quote('\n'.join(context._errors))

    return resp

if __name__ == "__main__":
    from waitress import serve
    serve(app, host="0.0.0.0", port=3000)