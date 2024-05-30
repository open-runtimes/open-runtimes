require 'sinatra'
require 'json'
require 'timeout'

USER_CODE_PATH = '/usr/local/server/src/function';

class RuntimeResponse
  def send(body, status_code = 200, headers = {})
    {
      'body' => body,
      'statusCode' => status_code,
      'headers' => headers
    }
  end

  def json(obj, status_code = 200, headers = {})
    headers['content-type'] = 'application/json'

    self.send(obj.to_json, status_code, headers)
  end

  def empty()
    self.send('', 204, {})
  end

  def redirect(url, status_code = 301, headers = {})
    headers['location'] = url

    self.send('', status_code, headers)
  end
end

class RuntimeRequest
  attr_accessor :body_raw
  attr_accessor :body
  attr_accessor :headers
  attr_accessor :method
  attr_accessor :url
  attr_accessor :path
  attr_accessor :port
  attr_accessor :scheme
  attr_accessor :host
  attr_accessor :query
  attr_accessor :query_string

  def initialize(url, method, scheme, host, port, path, query, query_string, headers, body, body_raw)
    @body_raw = body_raw
    @body = body
    @headers = headers
    @method = method
    @url = url
    @path = path
    @port = port
    @scheme = scheme
    @host = host
    @query = query
    @query_string = query_string
  end
end

class RuntimeContext
  attr_accessor :req
  attr_accessor :res
  attr_accessor :logs
  attr_accessor :errors

  def initialize(req, res)
    @req = req
    @res = res
    @logs = []
    @errors = []
  end

  def log(message)
    if message.kind_of?(Array) || message.kind_of?(Hash)
      @logs.push(message.to_json)
    else
      @logs.push(message.to_s)
    end
  end

  def error(message)
    if message.kind_of?(Array) || message.kind_of?(Hash)
      @errors.push(message.to_json)
    else
      @errors.push(message.to_s)
    end
  end
end

def action(request, response)
  safe_timeout = nil

  if request.env.key?('HTTP_X_OPEN_RUNTIMES_TIMEOUT')
    timeout = request.env['HTTP_X_OPEN_RUNTIMES_TIMEOUT'].to_i

    if timeout == 0
      response.status = 500
      response.body = 'Header "x-open-runtimes-timeout" must be an integer greater than 0.'
      return
    end

    safe_timeout = timeout
  end

  secret = request.env['HTTP_X_OPEN_RUNTIMES_SECRET'] || ''
  server_secret = ENV['OPEN_RUNTIMES_SECRET'] || ''

  if !(server_secret.empty?) && secret != server_secret
    response.status = 500
    response.body = 'Unauthorized. Provide correct "x-open-runtimes-secret" header.'
    return
  end

  request.body.rewind

  host = request.host

  scheme = request.scheme || 'http'
  default_port = scheme === 'https' ? 443 : 80
  port = request.port || default_port
  path = request.path
  query = {}
  query_string = request.query_string || ''

  if query_string.start_with?("?")
    query_string = query_string[1..-1]
  end

  url = scheme + "://" + host

  if port != default_port
    url += ':' + port.to_s
  end

  url += path

  unless query_string.empty?
    url += "?" + query_string

    query_string.split('&') do |param|
      pair = param.split('=', 2)

      if pair[0] != nil && !(pair[0].empty?)
        query[pair[0]] = pair[1]
      end
    end
  end

  body_raw = request.body.read
  body = body_raw
  method = request.request_method
  headers = {}

  unless request.env['CONTENT_TYPE'].nil?
    headers['content-type'] = request.env['CONTENT_TYPE']
  end

  unless request.env['CONTENT_LENGTH'].nil?
    headers['content-length'] = request.env['CONTENT_LENGTH']
  end

  request.env.each do |header, value|
    if header.start_with?('HTTP_')
      header = header[5..-1].gsub("_", "-").downcase

      unless header.start_with?('x-open-runtimes-')
        headers[header] = value
      end
    end
  end

  content_type = request.env['CONTENT_TYPE']
  content_type = 'text/plain' if content_type.nil?

  if content_type.include?('application/json')
    unless body_raw.empty?
      body = JSON.parse(body_raw)
    end
  end

  context_req = RuntimeRequest.new(url, method, scheme, host, port, path, query, query_string, headers, body, body_raw)
  context_res = RuntimeResponse.new
  context = RuntimeContext.new(context_req, context_res)

  custom_std = nil
  
  output = nil

  begin
    load(USER_CODE_PATH + '/' + ENV['OPEN_RUNTIMES_ENTRYPOINT'])

    unless defined?(main = ())
      raise 'User function is not valid.'
    end

    system_out = $stdout
    system_err = $stderr
    custom_std = StringIO.new
    $stdout = custom_std
    $stderr = custom_std

    unless safe_timeout.nil?
      executed = true

      task_thread = Thread.new do
        output = main(context)
      end

      begin
        Timeout.timeout(safe_timeout) do
          task_thread.join
        end
      rescue Timeout::Error
        executed = false
      end

      unless executed
        context.error('Execution timed out.')
        output = context.res.send('', 500, {})
      end
    else
      output = main(context)
    end
  rescue Exception => e
    context.error(e)
    context.error(e.backtrace.join("\n"))
    output = context.res.send('', 500, {})
  ensure
    $stdout = system_out
    $stderr = system_err
  end

  if output.nil?
    context.error('Return statement missing. return context.res.empty() if no response is expected.')
    output = context.res.send('', 500, {})
  end

  output['body'] = '' if output['body'].nil?
  output['statusCode'] = 200 if output['statusCode'].nil?
  output['headers'] = {} if output['headers'].nil?

  output['headers'].each do |header, value|
    header_str = header.to_s.downcase

    unless header_str.start_with?('x-open-runtimes-')
      response.headers[header_str] = value
    end
  end

  unless custom_std.string.nil? || custom_std.string.empty?
    context.log('----------------------------------------------------------------------------')
    context.log('Unsupported logs detected. Use context.log() or context.error() for logging.')
    context.log('----------------------------------------------------------------------------')
    context.log(custom_std.string)
    context.log('----------------------------------------------------------------------------')
  end

  response.headers['x-open-runtimes-logs'] = ERB::Util.url_encode(context.logs.join('\n'))
  response.headers['x-open-runtimes-errors'] = ERB::Util.url_encode(context.errors.join('\n'))

  response.headers['content-type'] = 'text/plain' if response.headers['content-type'].nil?

  unless response.headers['content-type'].start_with?('multipart/') || response.headers['content-type'].include?('charset=') 
    response.headers['content-type'] += '; charset=utf-8'
  end

  response.status = output['statusCode']
  response.body = output['body']
  response
end

def handle(request, response)
  begin
    action(request, response)
    response
  rescue Exception => e
    logs = []
    errors = [
      e,
      e.backtrace.join("\n")
    ]

    response.headers['x-open-runtimes-logs'] = ERB::Util.url_encode(logs.join('\n'))
    response.headers['x-open-runtimes-errors'] = ERB::Util.url_encode(errors.join('\n'))
    response.headers['content-type'] = 'text/plain'
    response.status = 500
    response.body = ''
    response
  end
end

get '*' do
  handle(request, response)
end

post '*' do
  handle(request, response)
end

put '*' do
  handle(request, response)
end

patch '*' do
  handle(request, response)
end

delete '*' do
  handle(request, response)
end

options '*' do
  handle(request, response)
end

error do
  status 500

  if ENV.key?('sinatra.error')
    ENV['sinatra.error'].message.to_json
  end
end
