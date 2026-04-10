require 'async'

def execute(safe_timeout, main, context)
  if safe_timeout.nil? || safe_timeout <= 0
    return [true, main.call(context)]
  end

  output = nil
  executed = true

  Async do |task|
    task.with_timeout(safe_timeout) do
      output = main.call(context)
    rescue Async::TimeoutError
      executed = false
    end
  end.wait

  [executed, output]
end
