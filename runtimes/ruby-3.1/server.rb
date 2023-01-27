require 'sinatra'
require 'json'

USER_CODE_PATH = '/usr/code-start';

class Response
  def send(body, statusCode = 200, headers = {})
    return {
      body: body,
      statusCode: statusCode,
      headers: headers
    }
  end

  def json(obj, statusCode = 200, headers = {})
    headers['content-type'] = 'application/json'
    return self.send(obj.to_json, statusCode, headers)
  end

  def empty()
    return self.send('', 200, {})
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
  def initialize(req, res)
    @res = req
    @res = res
  end

  attr_accessor :req
  attr_accessor :res

  @_logs = []
  @_errors = []

  # TODO: Support for infinite parameters
  # TODO: Support for objects (stringify)
  def log(message)
    @_logs.append(message)
  end

  def error(message)
    @_errors.append(message)
  end
end

def handle(request, response)
  request.body.rewind

  rawBody = request.body.read
  body = rawBody
  method = request.request_method
  url = request.path
  headers = {}

  contextReq = Request.new(rawBody, body, headers, method, url)
  contextRes = Response.new
  context = Context.new(contextReq, contextRes)

  response.body = {
    method: context.req.method,
    url: context.req.url,
    headers: context.req.headers,
    body: context.req.body,
    rawBody: context.req.rawBody
  }.to_json
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
  return { stderr: env['sinatra.error'].message }.to_json
end
