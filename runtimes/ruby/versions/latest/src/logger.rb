require 'securerandom'

class RuntimeLogger
  TYPE_LOG = "log"
  TYPE_ERROR = "error"

  attr_accessor :enabled
  attr_accessor :id
  attr_accessor :includes_native_info
  attr_accessor :stream_logs
  attr_accessor :stream_errors
  attr_accessor :custom_std_stream
  attr_accessor :native_logs_cache
  attr_accessor :native_errors_cache

  def initialize(status, id)
    @includes_native_info = false

    @enabled = false

    if status.nil? || status.empty? || status === "enabled"
      @enabled = true
    end

    if @enabled === true
      serverEnv = ENV['OPEN_RUNTIMES_ENV'] || ""

      if id.nil? || id.empty?
        if serverEnv === "development"
          @id = "dev"
        else
          @id = self.generate_id
        end
      else
        @id = id
      end

      @stream_logs = File.open("/mnt/logs/" + @id + "_logs.log", 'a')
      @stream_errors = File.open("/mnt/logs/" + @id + "_errors.log", 'a')
    end
  end

  def write(messages, type = RuntimeLogger::TYPE_LOG, native = false)
    if !@enabled
      return
    end

    if native && !@includes_native_info
      @includes_native_info = true
      self.write(["Native logs detected. Use context.log() or context.error() for better experience."], type,
                 native)
    end

    stream = @stream_logs

    if type === RuntimeLogger::TYPE_ERROR
      stream = @stream_errors
    end

    string_log = ""
    i = 0
    messages.each do |message|
      if message.kind_of?(Array) || message.kind_of?(Hash)
        string_log += message.to_json
      else
        string_log += message.to_s
      end

      if i < messages.length() - 1
        string_log += " ";
      end

      i += 1
    end

    try {
      stream.write(string_log)
    } catch (Exception e) {
      # Silently fail to prevent 500 errors in runtime
      # Log write failures should not crash the runtime
    }
  end

  def end()
    if !@enabled
      return
    end

    @enabled = false

    @stream_logs.close
    @stream_errors.close
  end

  def override_native_logs()
    @native_logs_cache = $stdout
    @native_errors_cache = $stderr
    @custom_std_stream = StringIO.new
    $stdout = @custom_std_stream
    $stderr = @custom_std_stream
  end

  def revert_native_logs()
    $stdout = @native_logs_cache
    $stderr = @native_errors_cache

    unless @custom_std_stream.string.nil? || @custom_std_stream.string.empty?
      self.write([custom_std_stream.string], RuntimeLogger::TYPE_LOG, true)
    end
  end

  def generate_id(padding = 7)
    now = Time.now
    sec = now.to_i
    usec = now.usec
    base_id = "%08x%05x" % [sec, usec]
    random_padding = SecureRandom.hex(padding)
    random_padding = random_padding[0...padding]
    base_id + random_padding
  end
end
