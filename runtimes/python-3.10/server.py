from flask import Flask, request, jsonify
import traceback
import pathlib
import os
import importlib.util

app = Flask(__name__);

class Response:
    def send(self, text):
        return text;
    def json(self, obj):
        return jsonify(obj);

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
            self.payload = {};


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
    if requestData['path'] is not None and requestData['file'] is not None:
        fullPath = pathlib.Path(requestData['path']);

        userFunction = importlib.machinery.SourceFileLoader('module.userfunc', str(fullPath) + '/' + requestData['file']).load_module()
        # Check if function exists
        if userFunction is None:
            return {'message': 'function not found, Did you forget to name it `main`?', 'code': 500}, 500;

        try:
            return userFunction.main(req, resp);
        except Exception as e:
            return {'message': str("".join(traceback.TracebackException.from_exception(e).format())), 'code': 500}, 500;
    else:
        return {'message': 'no path or file specified', 'code': 500}, 500;