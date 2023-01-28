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
  return self.send(obj.to_json, statusCode, headers)
  end
end

class Request
  def initialize(rawBody, body, headers, method, url)
    @rawBody = rawBody
    @body = body
    @headers = headers
    @method = method
    @url = url
  end

  attr_accessor :rawBody
  attr_accessor :body
  attr_accessor :headers
  attr_accessor :method
  attr_accessor :url
end

class Context
  def initialize(rq, rs)
    @req = rq
    @res = rs
    @_logs = []
    @_errors = []
  end

  attr_accessor :req
  attr_accessor :res
  attr_accessor :_logs
  attr_accessor :_errors

  # TODO: Support for infinite parameters
  # TODO: Support for objects (stringify)
  def log(message)
    @_logs.push(message)
  end

  def error(message)
    @_errors.push(message)
  end
end

def handle(request, response)
  secret = request.env['HTTP_X_OPEN_RUNTIMES_SECRET']
  if(secret == nil)
    secret = ''
  end

  if(secret != ENV['OPEN_RUNTIMES_SECRET'])
    response.status = 500
    response.body = 'Unauthorized. Provide correct "x-open-runtimes-secret" header.'
    return
  end

  request.body.rewind

  params = request.url.split(request.path)[1]
  url = request.path
  if(params != nil)
    url += params
  end

  rawBody = request.body.read
  body = rawBody
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
    body = JSON.parse(rawBody)
  end

  contextReq = Request.new(rawBody, body, headers, method, url)
  contextRes = Response.new
  context = Context.new(contextReq, contextRes)

  system_out = $customstd
  system_err = $customstd
  customstd = StringIO.new
  $customstd = customstd

  output = nil

  begin
    load(USER_CODE_PATH + '/' + ENV['OPEN_RUNTIMES_ENTRYPOINT'])

    unless defined?(main = ())
      raise 'User function is not valid.'
    end

    output = main(context)
    # TODO: Re-define system_out again. Same for all runtimes
  rescue Exception => e
    context.error(e)
    context.error(e.backtrace.join("\n"))
    output = context.res.send('', 500, {})
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

  output['headers'] do |header, value|
    if !header.downcase.start_with?('x-open-runtimes-')
      response.headers[header.downcase] = value
    end
  end

  if customstd.string != nil && !customstd.string.empty?
    context.log('Unsupported log noticed. Use context.log() or context.error() for logging.')
  end

  response.headers['x-open-runtimes-logs'] = ERB::Util.url_encode(context._logs.join('\n'))
  response.headers['x-open-runtimes-errors'] = ERB::Util.url_encode(context._errors.join('\n'))

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
