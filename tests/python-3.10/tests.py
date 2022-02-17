import json
import requests

#    'req' variable has:
#        'headers' - object with request headers
#        'payload' - object with request body data
#        'env' - object with environment variables
#    'res' variable has:
#        'send(text, status)' - function to return text response. Status code defaults to 200
#        'json(obj, status)' - function to return JSON response. Status code defaults to 200
#    
#    If an error is thrown, a response with code 500 will be returned.

def main(req, res):
    payload = json.load(req.payload)
    todo_id = payload.get('id', 1)

    header_data = req.headers.get('x-test-header', None)
    env_data = req.headers.env('test-env', None)

    todo = (requests.get('https://jsonplaceholder.typicode.com/todos/' + todo_id)).json()
    return res.json({
        'isTest': True,
        'message': 'Hello Open Runtimes 👋',
        'todo': todo,
        'header': header_data,
        'env': env_data
    })