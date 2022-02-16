require 'HTTParty'
require 'json'

def main(request, response)
    return response.json({:n => rand()})
end