def awaited_sleep()
    Async do
        sleep(3)
    end.wait
end