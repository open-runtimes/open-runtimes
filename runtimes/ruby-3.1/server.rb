require 'sinatra'
require 'json'

USER_CODE_PATH = '/usr/code-start';

class Response
  def send(body, statusCode = 200, headers = {})
    return {
      'body' => body,
      'statusCode' => statusCode,
      'headers' => headers
    }
  end

  def json(obj, statusCode = 200, headers = {})
    headers['content-type'] = 'application/json'
    return self.send(obj.to_json, statusCode, headers)
  end

  def empty()
    return self.send('', 204, {})
  end

  def redirect(url, statusCode = 301, headers = {})
    headers['location'] = url
    return self.send('', statusCode, headers)
  end
end

class Request
  def initialize(bodyString, body, headers, method, url, path, port, scheme, host, query, queryString)
    @bodyString = bodyString
    @body = body
    @headers = headers
    @method = method
    @url = url
    @path = path
    @port = port
    @scheme = scheme
    @host = host
    @query = query
    @queryString = queryString
  end

  attr_accessor :bodyString
  attr_accessor :body
  attr_accessor :headers
  attr_accessor :method
  attr_accessor :url
  attr_accessor :path
  attr_accessor :port
  attr_accessor :scheme
  attr_accessor :host
  attr_accessor :query
  attr_accessor :queryString
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
    if(message.kind_of?(Array) || message.kind_of?(Hash))
      @logs.push(message.to_json)
    else
      @logs.push(message.to_s)
    end
  end

  def error(message)
    if(message.kind_of?(Array) || message.kind_of?(Hash))
      @errors.push(message.to_json)
    else
      @errors.push(message.to_s)
    end
  end
end

def handle(request, response)
  secret = request.env['HTTP_X_OPEN_RUNTIMES_SECRET'] || ''
  serverSecret = ENV['OPEN_RUNTIMES_SECRET'] || ''

  if(secret == '' || secret != serverSecret)
    response.status = 500
    response.body = 'Unauthorized. Provide correct "x-open-runtimes-secret" header.'
    return
  end

  request.body.rewind

  host = request.host

  scheme = request.scheme || 'http'
  defaultPort = scheme === 'https' ? '443' : '80'
  port = request.port || defaultPort.to_i
  path = request.path
  query = {}
  queryString = request.query_string || ''

  if(queryString.start_with?("?"))
    queryString = queryString[1..-1]
  end

  url = scheme + "://" + host

  if(port != defaultPort.to_i)
    url += ':' + port.to_s
  end

  url += path

  if(!(queryString.empty?))
    url += "?" + queryString
  end


  if(!(queryString.empty?))
    queryString.split('&') do |param|
      pair = param.split('=', 2)
      if(pair[0] != nil && !(pair[0].empty?))
        query[pair[0]] = pair[1]
      end
    end
  end

  bodyString = request.body.read
  body = bodyString
  method = request.request_method
  headers = {}

  if(request.env['CONTENT_TYPE'] != nil)
    headers['content-type'] = request.env['CONTENT_TYPE']
  end

  if(request.env['CONTENT_LENGTH'] != nil)
    headers['content-length'] = request.env['CONTENT_LENGTH']
  end

  request.env.each do |header, value|
    if(header.start_with?('HTTP_'))
      header = header[5..-1]
      header = header.gsub("_", "-")
      header = header.downcase

      if(!header.start_with?('x-open-runtimes-'))
        headers[header] = value
      end
    end
  end

  contentType = request.env['CONTENT_TYPE']
  if(contentType == nil)
    contentType = 'text/plain'
  end

  if(contentType.include?('application/json'))
    if(!(bodyString.empty?))
      body = JSON.parse(bodyString)
    end
  end

  contextReq = Request.new(bodyString, body, headers, method, url, path, port, scheme, host, query, queryString)
  contextRes = Response.new
  context = Context.new(contextReq, contextRes)

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

  if(output['body'] == nil)
    output['body'] = ''
  end

  if(output['statusCode'] == nil)
    output['statusCode'] = 200
  end

  if(output['headers'] == nil)
    output['headers'] = {}
  end

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
end

get '*' do
  handle(request, response)
  return response
end

post '*' do
  handle(request, response)
  return response
end

put '*' do
  handle(request, response)
  return response
end

patch '*' do
  handle(request, response)
  return response
end

delete '*' do
  handle(request, response)
  return response
end

options '*' do
  handle(request, response)
  return response
end

error do
  status 500
  return env['sinatra.error'].message.to_json
end
