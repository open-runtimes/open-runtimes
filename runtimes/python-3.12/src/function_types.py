from logger import Logger
import json

class Response:
    def binary(self, bytes, statusCode = 200, headers = {}):
        return {
            'body': bytes,
            'statusCode': statusCode,
            'headers': headers,
        }
    
    def text(self, body, statusCode = 200, headers = {}):
        return self.binary(body.encode("utf-8"), statusCode, headers)

    def send(self, body, statusCode = 200, headers = {}):
        return self.text(str(body), statusCode, headers)

    def json(self, obj, statusCode = 200, headers = {}):
        headers['content-type'] = 'application/json'
        return self.text(json.dumps(obj, separators=(',', ':')), statusCode, headers)
    
    def empty(self):
        return self.text('', 204, {})

    def redirect(self, url, statusCode = 301, headers = {}):
        headers['location'] = url
        return self.text('', statusCode, headers)

class Request:
    body_binary = None
    headers = None
    method = None
    url = None
    path = None
    port = None
    query = None
    query_string = None
    scheme = None
    host = None

    @property
    def body_text(self):
        return self.body_binary.decode("utf-8")

    @property
    def body_json(self):
        return json.loads(self.body_text)

    @property
    def body_raw(self):
        return self.body_text

    @property
    def body(self):
        content_type = self.headers.get('content-type', 'text/plain').lower()

        if content_type.startswith('application/json'):
            if len(self.body_binary) > 0:
                return self.body_json
            else:
                return {}

        binary_types = ["application/", "audio/", "font/", "image/", "video/"]
        for binary_type in binary_types:
            if content_type.startswith(binary_type):
                return self.body_binary

        return self.body_text

class Context:
    req = Request()
    res = Response()
    logger = None

    logs = []
    errors = []

    def __init__(self, logger):
        self.logs = []
        self.errors = []
        self.req = Request()
        self.res = Response()
        self.logger = logger

    def log(self, message):
        self.logger.write(message, Logger.TYPE_LOG)

    def error(self, message):
        self.logger.write(message, Logger.TYPE_ERROR)
