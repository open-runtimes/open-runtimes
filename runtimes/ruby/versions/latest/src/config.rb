require 'json'

module Config
  SECRET = (ENV['OPEN_RUNTIMES_SECRET'] || '').freeze
  ENTRYPOINT = ENV['OPEN_RUNTIMES_ENTRYPOINT'].freeze
  ENV_NAME = (ENV['OPEN_RUNTIMES_ENV'] || "").freeze
  # Inline logs travel on the response, so no log files are written
  INLINE_LOGS = ENV['OPEN_RUNTIMES_LOGGING'] == 'inline'
  LOGS_DIRECTORY = (INLINE_LOGS ? '' : (ENV['OPEN_RUNTIMES_LOGS_DIRECTORY'] || '/mnt/logs')).freeze
  HEADERS = begin
    JSON.parse(ENV['OPEN_RUNTIMES_HEADERS'] || '{}')
  rescue JSON::ParserError
    {}
  end.freeze
end
