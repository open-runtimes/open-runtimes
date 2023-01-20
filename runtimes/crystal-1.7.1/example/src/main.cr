require "http/client"
require "json"

module App
  # 'req' variable has:
  #     'headers' - object with request headers
  #     'payload' - object with request body data
  #     'variables' - object with function variables
  # 'res' variable has:
  #     'send(text, status)' - function to return text response. Status code defaults to 200
  #     'json(obj, status)' - function to return JSON response. Status code defaults to 200
  #
  # If an error is thrown, a response with code 500 will be returned.
  def self.exec(req, res)
    payload = JSON.parse(req.payload)
    id = payload.as_h.fetch("id", 1)

    response = HTTP::Client.get("https://jsonplaceholder.typicode.com/todos/" + id.to_s)
    todo = JSON.parse(response.body)

    return res.json({
      "message" => "Hello Open Runtimes 👋",
      "todo"    => todo,
    })
  end
end
