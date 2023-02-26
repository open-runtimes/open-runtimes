require 'sinatra'
require 'json'

USER_CODE_PATH = '/usr/code-start';

class RuntimeResponse
  def send(body, status_code = 200, headers = {})
    return {
      'body' => body,
      'statusCode' => status_code,
      'headers' => headers
    }
  end

  def json(obj, status_code = 200, headers = {})
    headers['content-type'] = 'application/json'
    return self.send(obj.to_json, status_code, headers)
  end

  def empty()
    return self.send('', 204, {})
  end

  def redirect(url, status_code = 301, headers = {})
    headers['location'] = url
    return self.send('', status_code, headers)
  end
end

class RuntimeRequest
  def initialize(url, method, scheme, host, port, path, query, query_string, headers, body, body_string)
    @body_string = body_string
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

  attr_accessor :body_string
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
end

class Context
  def initialize(rq, rs)
    @req = rq
    @res = rs
    @logs = []
    @errors = []
  end

  attr_accessor :req
  attr_accessor :res
  attr_accessor :logs
  attr_accessor :errors

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

def handle(request, response)
  secret = request.env['HTTP_X_OPEN_RUNTIMES_SECRET'] || ''
  server_secret = ENV['OPEN_RUNTIMES_SECRET'] || ''

  if secret == '' || secret != server_secret
    response.status = 500
    response.body = 'Unauthorized. Provide correct "x-open-runtimes-secret" header.'
    return
  end

  request.body.rewind

  host = request.host

  scheme = request.scheme || 'http'
  default_port = scheme === 'https' ? '443' : '80'
  port = request.port || default_port.to_i
  path = request.path
  query = {}
  query_string = request.query_string || ''

  if query_string.start_with?("?")
    query_string = query_string[1..-1]
  end

  url = scheme + "://" + host

  if port != default_port.to_i
    url += ':' + port.to_s
  end

  url += path

  if !(query_string.empty?)
    url += "?" + query_string
  end


  if !(query_string.empty?)
    query_string.split('&') do |param|
      pair = param.split('=', 2)
      if pair[0] != nil && !(pair[0].empty?)
        query[pair[0]] = pair[1]
      end
    end
  end

  body_string = request.body.read
  body = body_string
  method = request.request_method
  headers = {}

  if request.env['CONTENT_TYPE'] != nil
    headers['content-type'] = request.env['CONTENT_TYPE']
  end

  if request.env['CONTENT_LENGTH'] != nil
    headers['content-length'] = request.env['CONTENT_LENGTH']
  end

  request.env.each do |header, value|
    if header.start_with?('HTTP_')
      header = header[5..-1].gsub("_", "-").downcase

      if !header.start_with?('x-open-runtimes-')
        headers[header] = value
      end
    end
  end

  content_type = request.env['CONTENT_TYPE']
  content_type = 'text/plain' if content_type.nil?

  if content_type.include?('application/json')
    if !(body_string.empty?)
      body = JSON.parse(body_string)
    end
  end

  context_req = RuntimeRequest.new(url, method, scheme, host, port, path, query, query_string, headers, body, body_string)
  context_res = RuntimeResponse.new
  context = Context.new(context_req, context_res)

  customstd = nil
  
  output = nil

  begin
    load(USER_CODE_PATH + '/' + ENV['OPEN_RUNTIMES_ENTRYPOINT'])

    unless defined?(main = ())
      raise 'User function is not valid.'
    end

    system_out = $stdout
    system_err = $stderr
    customstd = StringIO.new
    $stdout = customstd
    $stderr = customstd

    output = main(context)
  rescue Exception => e
    context.error(e)
    context.error(e.backtrace.join("\n"))
    output = context.res.send('', 500, {})
  ensure
    $stdout = system_out
    $stderr = system_err
  end

  if output == nil
    context.error('Return statement missing. return context.res.empty() if no response is expected.')
    output = context.res.send('', 500, {})
  end


  output['body'] = '' if output['body'].nil?
  output['statusCode'] = 200 if output['statusCode'].nil?
  output['headers'] = {} if output['headers'].nil?

  output['headers'].each do |header, value|
    if !header.downcase.start_with?('x-open-runtimes-')
      response.headers[header.downcase] = value
    end
  end

  if customstd.string != nil && !customstd.string.empty?
    context.log('Unsupported log noticed. Use context.log() or context.error() for logging.')
  end

  response.headers['x-open-runtimes-logs'] = ERB::Util.url_encode(context.logs.join('\n'))
  response.headers['x-open-runtimes-errors'] = ERB::Util.url_encode(context.errors.join('\n'))

  if output['headers']['content-type'] != nil
    response.content_type = output['headers']['content-type']
  end

  response.status = output['statusCode']
  response.body = output['body']
  response
end

get '*' do
  return handle(request, response)
end

post '*' do
  return handle(request, response)
end

put '*' do
  return handle(request, response)
end

patch '*' do
  return handle(request, response)
end

delete '*' do
  return handle(request, response)
end

options '*' do
  return handle(request, response)
end

error do
  status 500
  return env['sinatra.error'].message.to_json
end
