require 'sinatra'
require 'json'

require_relative 'types.rb'
require_relative 'logger.rb'
require_relative 'execute.rb'

USER_CODE_PATH = '/usr/local/server/src/function';

def action(request, response, logger)
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

  unless request.env['CONTENT_LENGTH'].nil?
    headers['content-length'] = request.env['CONTENT_LENGTH']
  end

  max_size = 20 * 1024 * 1024

  content_length = request.env['CONTENT_LENGTH']
  if content_length.nil?
    content_length = headers['content-length']
  end

  if !(content_length.nil?) && content_length.to_i > max_size
    raise 'Request body size exceeds the size limit.'
  end

  body_binary = request.body.read

  if body_binary.length > max_size
    raise 'Request body size exceeds the size limit.'
  end

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

  enforced_headers = JSON.parse(ENV['OPEN_RUNTIMES_HEADERS'].empty? ? '{}' : ENV['OPEN_RUNTIMES_HEADERS'])
  enforced_headers.each do |key, value|
    headers[key.downcase] = value.to_s
  end

  context_req = RuntimeRequest.new(url, method, scheme, host, port, path, query, query_string, headers,
                                   body_binary)
  context_res = RuntimeResponse.new
  context = RuntimeContext.new(context_req, context_res, logger)

  output = nil

  begin
    load(USER_CODE_PATH + '/' + ENV['OPEN_RUNTIMES_ENTRYPOINT'])

    unless defined?(main = ()) # rubocop:disable Lint/AssignmentInCondition, Lint/EmptyExpression
      raise 'User function is not valid.'
    end

    logger.override_native_logs

    unless safe_timeout.nil?
      results = execute(safe_timeout, main, context)
      executed = results[0]
      output = results[1]

      unless executed
        context.error('Execution timed out.')
        output = context.res.text('', 500, {})
      end
    else
      output = main(context)
    end
  rescue StandardError => e
    message = ""
    message += e.full_message
    message += "\n"
    message += e.backtrace.join("\n")

    context.error(message)
    output = context.res.text('', 500, {})
  ensure
    logger.revert_native_logs
  end

  if output.nil?
    context.error('Return statement missing. return context.res.empty() if no response is expected.')
    output = context.res.text('', 500, {})
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

  logger.end
  response.headers['x-open-runtimes-log-id'] = logger.id

  response.headers['content-type'] = 'text/plain' if response.headers['content-type'].nil?
  response.headers['content-type'] = response.headers['content-type'].downcase

  is_multipart = response.headers['content-type'].start_with?('multipart/')
  has_charset = response.headers['content-type'].include?('charset=')
  unless is_multipart || has_charset
    response.headers['content-type'] += '; charset=utf-8'
  end

  response.status = output['statusCode']
  response.body = output['body']
  response
end

def handle(request, response)
  logger = RuntimeLogger.new(request.env['HTTP_X_OPEN_RUNTIMES_LOGGING'], request.env['HTTP_X_OPEN_RUNTIMES_LOG_ID'])
  begin
    action(request, response, logger)
    response
  rescue StandardError => e
    message = ""
    message += e.full_message
    message += "\n"
    message += e.backtrace.join("\n")

    logger.write([message], RuntimeLogger::TYPE_ERROR)
    logger.end

    response.headers['x-open-runtimes-log-id'] = logger.id
    response.headers['content-type'] = 'text/plain'
    response.status = 500
    response.body = ''
    response
  end
end

before do
    if request.path == '/__opr/health'
        headers['content-type'] = 'text/plain'
        halt 200, "OK"
    end
  if request.path == '/__opr/timings'
    timings = File.read('/mnt/telemetry/timings.txt')
    headers['content-type'] = 'text/plain; charset=utf-8'
    halt 200, timings
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

puts "HTTP server successfully started!"
