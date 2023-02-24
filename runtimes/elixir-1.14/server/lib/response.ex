defmodule Server.Response do

  def send(text, status \\ 200) do
    %{ status: status, response: text }
  end

  def json(body, status \\ 200) do
    %{ status: status, response: body }
  end

end
