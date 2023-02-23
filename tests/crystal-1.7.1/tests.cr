require "http/client"
require "json"

module Handler
  def self.main(req, res)
    payload = JSON.parse(req.payload)
    id = payload.as_h.fetch("id", 1)

    response = HTTP::Client.get("https://jsonplaceholder.typicode.com/todos/" + id.to_s)
    todo = JSON.parse(response.body)

    puts "String1"
    puts 42
    puts 4.2
    puts true

    puts "String2"
    puts "String3"
    puts "String4"
    puts "String5"
    
    return res.json({
        "isTest" => true,
        "message" => "Hello Open Runtimes 👋",
        "todo" => todo,
        "header" => req.headers["x-test-header"]?,
        "variable" => req.variables["test-variable"]?
    })
  end
end
