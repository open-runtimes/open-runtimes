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
    todo = (requests.get('https://jsonplaceholder.typicode.com/todos/1', auth=('user', 'pass'))).json()
    return res.json({
        'message': 'Hello Open Runtimes 👋',
        'todo': todo
    })