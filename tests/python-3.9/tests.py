import json
import requests

#    'req' variable has:
#        'headers' - object with request headers
#        'payload' - object with request body data
#        'variables' - object with function variables
#    'res' variable has:
#        'send(text, status)' - function to return text response. Status code defaults to 200
#        'json(obj, status)' - function to return JSON response. Status code defaults to 200
#    
#    If an error is thrown, a response with code 500 will be returned.

def main(req, res):
    payload = json.loads('{}' if not req.payload else req.payload)
    todo_id = payload.get('id', 1)

    header_data = req.headers.get('x-test-header', None)
    var_data = req.variables.get('test-variable', None)

    todo = (requests.get('https://jsonplaceholder.typicode.com/todos/' + str(todo_id))).json()

    print('String')
    print(42)
    print(4.2)
    print(True)

    print("String2")
    print("String3")
    print("String4")
    print("String5")

    return res.json({
        'isTest': True,
        'message': 'Hello Open Runtimes 👋',
        'todo': todo,
        'header': header_data,
        'variable': var_data
    })