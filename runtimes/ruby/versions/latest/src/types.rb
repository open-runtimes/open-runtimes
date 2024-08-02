require_relative 'logger.rb'
require 'json'

class RuntimeResponse
  def binary(bytes, status_code = 200, headers = {})
    {
      'body' => bytes,
      'statusCode' => status_code,
      'headers' => headers
    }
  end

  def send(body, status_code = 200, headers = {})
    self.text(body.to_s, status_code, headers)
  end

  def text(body, status_code = 200, headers = {})
    self.binary(body, status_code, headers)
  end

  def json(obj, status_code = 200, headers = {})
    headers['content-type'] = 'application/json'

    self.text(obj.to_json, status_code, headers)
  end

  def empty()
    self.text('', 204, {})
  end

  def redirect(url, status_code = 301, headers = {})
    headers['location'] = url

    self.text('', status_code, headers)
  end
end

class RuntimeRequest
  attr_accessor :body_binary
  attr_accessor :headers
  attr_accessor :method
  attr_accessor :url
  attr_accessor :path
  attr_accessor :port
  attr_accessor :scheme
  attr_accessor :host
  attr_accessor :query
  attr_accessor :query_string

  def initialize(url, method, scheme, host, port, path, query, query_string, headers, body_binary)
    @body_binary = body_binary
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

  def body_text
    self.body_binary
  end

  def body_raw
    self.body_text
  end

  def body_json
    JSON.parse(self.body_text)
  end

  def body
    content_type = (@headers['content-type'] || 'text/plain').downcase

    if content_type.start_with?("application/json")
      if self.body_binary.empty?
        return {}
      else
        return self.body_json
      end
    end

    binary_types = ["application/", "audio/", "font/", "image/", "video/"]

    for binary_type in binary_types
      if content_type.start_with?(binary_type)
        return self.body_binary
      end
    end

    return self.body_text
  end
end

class RuntimeContext
  attr_accessor :req
  attr_accessor :res
  attr_accessor :logger

  def initialize(req, res, logger)
    @req = req
    @res = res
    @logger = logger
  end

  def log(message)
    @logger.write(message, RuntimeLogger::TYPE_LOG)
    @logger.write("\n", RuntimeLogger::TYPE_LOG)
  end

  def error(message)
    @logger.write(message, RuntimeLogger::TYPE_ERROR)
    @logger.write("\n", RuntimeLogger::TYPE_ERROR)
  end
end
