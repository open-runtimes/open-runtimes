require 'httparty'
require 'json'

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
    todo_id = JSON.parse(req.payload)['id']

    header_data = req.headers['x-test-header']
    env_data = req.headers['test-env']

    todo = HTTParty.get("https://jsonplaceholder.typicode.com/todos/#{todo_id}")
    return res.json({
        'isTest': true,
        'message': 'Hello Open Runtimes 👋',
        'todo': todo,
        'header': header_data,
        'env': env_data
    })
end