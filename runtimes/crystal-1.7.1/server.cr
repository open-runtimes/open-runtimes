require "json"
require "http/client"

require "grip"
require "./lib/runtime_user_code/{entrypoint}" # copied into lib directory

class RuntimeRequest
  getter payload : String
  getter variables : Hash(String, JSON::Any)
  getter headers : Hash(String, JSON::Any)

  def initialize(payload, variables, headers)
    @payload = payload || "{}"
    @variables = variables || {} of String => JSON::Any
    @headers = headers || {} of String => JSON::Any
  end
end

class RuntimeResponse
  getter response : String | JSON::Any | Nil

  def send(message)
    @response = message.to_s
  end

  def json(object)
    @response = JSON.parse(object.to_json)
  end
end

class RuntimeController < Grip::Controllers::Http
  def post(context : Context)
    challenge = context.get_req_header?("X-INTERNAL-CHALLENGE")
    return context.put_status(401).json({"stderr" => "Unauthorized"}) unless challenge

    if challenge != ENV["INTERNAL_RUNTIME_KEY"]
      return context.put_status(401).json({"stderr" => "Unauthorized"})
    end

    data = context.fetch_json_params
    request_data = RuntimeRequest.new(data["payload"]?.try(&.to_s), data["variables"]?.as(Hash(String, JSON::Any) | Nil), data["headers"]?.as(Hash(String, JSON::Any) | Nil))
    runtime_response = RuntimeResponse.new

    original_stdout = File.open("/dev/null")
    original_stdout.reopen(STDOUT)

    original_stderr = File.open("/dev/null")
    original_stderr.reopen(STDERR)

    begin
      stdout_reader, stdout_writer = IO.pipe
      stderr_reader, stderr_writer = IO.pipe
      STDERR.reopen(stderr_writer)
      STDOUT.reopen(stdout_writer)

      user_response = Handler.main(request_data, runtime_response)

      stdout_writer.close
      stderr_writer.close

      STDOUT.reopen(original_stdout)
      STDERR.reopen(original_stderr)

      user_stdout = stdout_reader.gets_to_end
      user_stderr = stderr_reader.gets_to_end

      return context.put_status(200).json({
        "response" => user_response,
        "stdout"   => user_stdout,
      })
    rescue e : Exception
      error_string = String.build do |str|
        str << user_stderr
        str << e.message
        e.backtrace.each do |line|
          str << line
        end
      end
      return context.put_status(500).json({
        "stdout" => user_stdout,
        "stderr" => error_string.strip,
      })
    ensure
      [stdout_reader, stderr_reader].each(&.try(&.close))
      STDOUT.reopen(original_stdout)
      STDERR.reopen(original_stderr)
    end
  end
end

class RuntimeApplication < Grip::Application
  def initialize(env : String, serve_static : Bool)
    super(env, serve_static)

    post "/", RuntimeController

    router.insert(0, Grip::Handlers::Log.new)
  end

  def port : Int32
    3000
  end
end

app = RuntimeApplication.new(ENV["GRIP_ENV"]? || "development", false)
app.run
