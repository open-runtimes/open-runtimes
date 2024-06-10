
require_relative 'logger.rb'
require 'json'

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
    attr_accessor :logger
  
    def initialize(req, res, logger)
      @req = req
      @res = res
      @logger = logger
    end
  
    def log(message)
      @logger.write(message, RuntimeLogger::TYPE_LOG)
    end
  
    def error(message)
        @logger.write(message, RuntimeLogger::TYPE_ERROR)
    end
  end