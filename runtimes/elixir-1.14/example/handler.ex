defmodule Handler do
  def main(_req, res) do
    # IO.inspect "REQ: #{Jason.encode!(req)}"
    # IO.inspect res
    # %{status: "one"}
    # res.send("one")
    # result = Map.put(req, "message", "Hello Open Runtimes 👋")
    res.send(:rand.uniform())
  end
end
