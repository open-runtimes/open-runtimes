def execute(safe_timeout, main, context) # rubocop:disable Lint/UnusedMethodArgument
  output = nil
  executed = true

  task_thread = Thread.new do
    output = main(context)
  end

  begin
    Timeout.timeout(safe_timeout) do
      task_thread.join
    end
  rescue Timeout::Error
    executed = false
  end

  return [executed, output]
end
