from flask import Flask, request, jsonify
from io import StringIO
import traceback
import pathlib
import os
import importlib
import sys

app = Flask(__name__)

class Response:
    _response = None
    _status = None

    def __init__(self):
        self._response = None
        self._status = None

    def send(self, text, status = 200):
        self._status = status
        self._response = text

    def json(self, obj, status = 200):
        self._status = status
        self._response = obj

class Request:

    def __init__(self, request):
        self.parsedRequest = request.get_json()

        if 'env' in self.parsedRequest:
            self.env = self.parsedRequest['env']
        else:
            self.env = {}

        if 'headers' in self.parsedRequest:
            self.headers = self.parsedRequest['headers']
        else:
            self.headers = {}

        if 'payload' in self.parsedRequest:
            self.payload = self.parsedRequest['payload']
        else:
            self.payload = ''


@app.route('/', defaults={'u_path': ''}, methods = ['POST'])
@app.route('/<path:u_path>', methods = ['POST'])
def handler(u_path):

    if (request.headers.get('x-internal-challenge') != os.getenv('INTERNAL_RUNTIME_KEY')):
        return 'Unauthorized', 500

    requestData = request.get_json()

    if requestData is None:
        return 'No data received', 500

    # Create new request and response object
    req = Request(request)
    resp = Response()

    # Import function from request
    userPath = os.getenv('INTERNAL_RUNTIME_ENTRYPOINT')
    if userPath.endswith('.py'):
        size = len(userPath)
        userPath = userPath[:size - 3]
    userPath = userPath.replace("/", ".")
    userModule = importlib.import_module("userlib." + userPath)

    # Check if function exists
    if userModule is None:
        return 'Function not found, Did you forget to name it `main`?', 500

    try:
        sys.stdout = userout = StringIO()
        sys.stderr = usererr = StringIO()

        userModule.main(req, resp)

        output = {
            "response": resp._response,
            "stdout": userout.getvalue()
        }
        return jsonify(output), resp._status
    except Exception as e:
        output = {
            "stdout": userout.getvalue(),
            "stderr": usererr.getvalue() + "\n" + traceback.TracebackException.from_exception(e).format()
        }
        return jsonify(output), 500
    finally:
        sys.stdout = sys.__stdout__
        sys.stderr = sys.__stderr__

if __name__ == "__main__":
    from waitress import serve
    serve(app, host="0.0.0.0", port=3000, threads=100)