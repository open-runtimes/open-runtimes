require 'json'

module Config
  SECRET = (ENV['OPEN_RUNTIMES_SECRET'] || '').freeze
  ENTRYPOINT = ENV['OPEN_RUNTIMES_ENTRYPOINT'].freeze
  ENV_NAME = (ENV['OPEN_RUNTIMES_ENV'] || "").freeze
  HEADERS = begin
    JSON.parse(ENV['OPEN_RUNTIMES_HEADERS'] || '{}')
  rescue JSON::ParserError
    {}
  end.freeze
end
