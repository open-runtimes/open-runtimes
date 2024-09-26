def awaited_sleep()
  thread = Thread.new do
    sleep(3)
  end
  thread.join
end
