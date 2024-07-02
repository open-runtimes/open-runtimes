require 'async'

def execute(safe_timeout, main, context)
    output = nil
    executed = true
  
    Async do |task|
      task.with_timeout(safe_timeout) do
        output = main(context)
      rescue Async::TimeoutError
        executed = false
      end
    end.wait
  
    return [ executed, output ]
end