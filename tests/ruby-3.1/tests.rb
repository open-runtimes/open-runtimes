require 'httparty'
require 'json'

#    'req' variable has:
#        'headers' - object with request headers
#        'payload' - object with request body data
#        'variables' - object with function variables
#    'res' variable has:
#        'send(text, status)' - function to return text response. Status code defaults to 200
#        'json(obj, status)' - function to return JSON response. Status code defaults to 200
#    
#    If an error is thrown, a response with code 500 will be returned.

def main(req, res)
    payload = JSON.parse(req.payload === '' ? '{}' : req.payload)

    todo = JSON.parse(HTTParty.get("https://jsonplaceholder.typicode.com/todos/" + (payload['id'] || '1')).body)

    puts 'String1'
    puts 42
    puts 4.2
    puts true

    puts "String2"
    puts "String3"
    puts "String4"
    puts "String5"
    
    return res.json({
        'isTest': true,
        'message': 'Hello Open Runtimes 👋',
        'todo': todo,
        'header': req.headers['x-test-header'],
        'variable': req.variables['test-variable']
    })
end