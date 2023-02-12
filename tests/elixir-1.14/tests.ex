#    'req' variable has:
#        'headers' - object with request headers
#        'payload' - object with request body data
#        'variables' - object with function variables
#    'res' variable has:
#        'send(text, status)' - function to return text response. Status code defaults to 200
#        'json(obj, status)' - function to return JSON response. Status code defaults to 200
#
#    If an error is thrown, a response with code 500 will be returned.

defmodule HandlerTest do

  def main(req, res) do
    Application.ensure_all_started(:inets)

    Application.ensure_all_started(:ssl)

    payload = Jason.decode!(Map.take(req, ["payload"]))
    {status, todo} = :httpc.request(
      "https://jsonplaceholder.typicode.com/todos/" <> Map.get(payload, "id", "1")
    )

    IO.puts 'log1'
    IO.puts '{hello: world}'
    IO.puts '[hello, world]'

    res.json(%{
        isTest: true,
        message: "Hello Open Runtimes 👋",
        todo: todo,
        header: req.headers['x-test-header'],
        variable: req.variables['test-variable']
    })
  end

end
