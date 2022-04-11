from flask import Flask, request, jsonify
import traceback
import pathlib
import os
import importlib

app = Flask(__name__);

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
        self._response = jsonify(obj)

class Request:
    def __init__(self, request):
        self.parsedRequest = request.get_json();

        if 'env' in self.parsedRequest:
            self.env = self.parsedRequest['env'];
        else:
            self.env = {};

        if 'headers' in self.parsedRequest:
            self.headers = self.parsedRequest['headers'];
        else:
            self.headers = {};
        
        if 'payload' in self.parsedRequest:
            self.payload = self.parsedRequest['payload'];
        else:
            self.payload = '{}';


@app.route('/', defaults={'u_path': ''}, methods = ['POST'])
@app.route('/<path:u_path>', methods = ['POST'])
def handler(u_path):

    if (request.headers.get('x-internal-challenge') != os.getenv('INTERNAL_RUNTIME_KEY')):
        return {'message': 'Unauthorized', 'code': 401}, 401;

    requestData = request.get_json();

    if requestData is None:
        return {'message': 'no data received', 'code': 500}, 500;
    
    # Create new request and response object
    req = Request(request);
    resp = Response();

    # Import function from request
    userPath = os.getenv('INTERNAL_RUNTIME_ENTRYPOINT')
    if userPath.endswith('.py'):
        size = len(userPath)
        userPath = userPath[:size - 3]
    userPath = userPath.replace("/", ".")
    userModule = importlib.import_module("userlib." + userPath)

    # Check if function exists
    if userModule is None:
        return {'message': 'function not found, Did you forget to name it `main`?', 'code': 500}, 500

    try:
        userModule.main(req, resp)

        return resp._response, resp._status
    except Exception as e:
        return {'message': str("".join(traceback.TracebackException.from_exception(e).format())), 'code': 500}, 500

if __name__ == "__main__":
    from waitress import serve
    serve(app, host="0.0.0.0", port=3000)