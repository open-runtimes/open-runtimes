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

def main(req, res)
    payload = JSON.parse(req.payload === '' ? '{}' : req.payload)

    todo = JSON.parse(HTTParty.get("https://jsonplaceholder.typicode.com/todos/" + (payload['id'] || '1')).body)

    puts 'log1'
    puts '{hello: world}'
    puts '[hello, world]'
    
    return res.json({
        'isTest': true,
        'message': 'Hello Open Runtimes 👋',
        'todo': todo,
        'header': req.headers['x-test-header'],
        'env': req.env['test-env'],
        'null-env': req.env['null-env']
    })
end