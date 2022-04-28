require 'sinatra'
require 'json'

USER_CODE_PATH = '/usr/code-start';

before do
  content_type :json
end

class RuntimeRequest
  def initialize(payload = '{}', env = {}, headers = {})
    if payload == nil
      payload = ''
    end

    if headers == nil
      headers = {}
    end

    if env == nil
      env = {}
    end

    @payload = payload
    @env = env
    @headers = headers
  end

  def payload
    @payload
  end

  def env
    @env
  end

  def headers
    @headers
  end
end

class RuntimeResponse
  def send(message)
    @response = message
  end

  def json(object)
    @response = object.to_json
  end
end

post '/' do
  challenge = request.env['HTTP_X_INTERNAL_CHALLENGE'] || ''

  if challenge == ''
    status 500
    return 'Unauthorized'
  end

  if challenge != ENV['INTERNAL_RUNTIME_KEY']
    status 500
    return 'Unauthorized'
  end

  request.body.rewind
  data = JSON.parse(request.body.read)

  requestData = RuntimeRequest.new(data['payload'], data['env'], data['headers'])
  runtimeResponse = RuntimeResponse.new

  begin
    load(USER_CODE_PATH + '/' + ENV['INTERNAL_RUNTIME_ENTRYPOINT'])
  rescue Exception => e
    p e
    status 500
    return e.backtrace.join("\n")
  end

  unless defined?(main = ())
    status 500
    return 'File does not specify a main() function.'
  end

  system_out = $sdtout
  system_err = $stderr
  user_out = StringIO.new
  user_err = StringIO.new
  $stdout = user_out
  $stderr = user_err

  begin
    user_response = main(requestData, runtimeResponse)
    response = JSON.parse({
      'response' => user_response.response,
      'stdout' => user_out.string
    })
  rescue Exception => e
    status 500
    return JSON.parse({
      'stdout' => user_out.string
      'stderr' => user_err.string + "\n" + e.backtrace.join("\n")
    })
  ensure
    user_out = nil
    user_err = nil
    $stdout = system_out
    $stderr = system_err
  end

  status 200
  return response
end

error do
  status 500
  return env['sinatra.error'].message
end
