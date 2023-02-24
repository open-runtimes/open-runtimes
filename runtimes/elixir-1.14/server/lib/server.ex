defmodule Server do
  @moduledoc """
  Documentation for `Server`.
  """
  use Plug.Router

  use Plug.ErrorHandler

  plug :match

  plug Plug.Parsers,
       parsers: [:json],
       pass:  ["application/json"],
       json_decoder: Jason

  plug :dispatch

  post _ do
    # IO.inspect conn.body_params # Prints JSON POST body
    # try do
    #     Index.main(conn.body_params, Server.Response)
    # rescue
    #   e ->
    #   Logger.error(Exception.format(:error, e, __STACKTRACE__))
    #   reraise e, __STACKTRACE__
    # end

    # IO.inspect get_req_header(conn, "x-internal-challenge")
    # IO.inspect System.get_env("INTERNAL_RUNTIME_KEY")

    if [System.get_env("INTERNAL_RUNTIME_KEY")] == get_req_header(conn, "x-internal-challenge") do
      # IO.inspect Process.get()
      # IO.inspect Process.get(:standard_io)

      # Process.unregister(orig_stdio_pid)
      {:ok, dev} = StringIO.open("")
      # Process.register(dev, :stdio)

      pid = Process.group_leader()
      Process.group_leader(self(), dev)

      output = try do
        req = Map.take(conn.body_params, ["variables", "headers", "payload"])
        case Index.main(req, Server.Response) do
          o when
              is_map(o) and
              is_map_key(o, :status) and
              is_map_key(o, :response) -> o
          o -> %{
            status: 500,
            stderr: "Incorrect output from Index.main\n```#{Kernel.inspect(o)}```"
          }
        end
      catch
       _kind, value -> %{
         status: 500,
         stderr: "#{value}"
       }
      end

      Process.group_leader(self(), pid)
      # IO.inspect Process.get()

      {_, stdout} = StringIO.contents(dev)
      output = Map.put(output, :stdout, stdout)

      # IO.inspect(output)

      conn
      |> put_resp_header("content-type", "application/json")
      |> send_resp(
          output.status,
          Jason.encode!(
            Map.take(output, [:stdout, :stderr, :response])
          )
        )
    else
      send_resp(conn, 401, "Unauthorized")
    end

  end

  match _ do
    send_resp(conn, 404, "")
  end

  defp handle_errors(conn, %{kind: _kind, reason: reason, stack: _stack}) do
    # IO.inspect Exception.format(kind, reason, stack)
    # IO.inspect Exception.format_exit(reason)
    # IO.inspect Exception.message(reason)
    send_resp(conn, conn.status, Jason.encode!(%{
      stdout: "",
      stderr: Exception.message(reason)
    }))
  end
end
