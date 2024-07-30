def execute(safe_timeout, _main, context)
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
