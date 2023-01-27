require 'httparty'
require 'json'

def main(context)
    payload = context.req.body

    todo = JSON.parse(HTTParty.get('https://jsonplaceholder.typicode.com/todos/' + (payload['id'] || '1')).body)

    return context.res.json({
        :message => 'Hello Open Runtimes 👋',
        :todo => todo
    })
end