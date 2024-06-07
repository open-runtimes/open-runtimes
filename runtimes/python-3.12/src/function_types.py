from logger import Logger
import json

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
    body_raw = None
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
        logger.write(message, Logger.TYPE_LOG)

    def error(self, message):
        logger.write(message, Logger.TYPE_ERROR)
