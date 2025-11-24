#!/bin/bash
# Fail if any command fails
set -e

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[32m Starting MySQL Runtime... \e[0m"

# Ensure telemetry directory exists
mkdir -p /mnt/telemetry
mkdir -p /mnt/logs

# Record startup time
start=$(awk '{print $1}' /proc/uptime)

# Start the OpenRuntimes management server (which starts MySQL)
cd /usr/local/server
node src/server.js &

# Wait for MySQL to be ready
echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Waiting for MySQL to be ready... \e[0m"

max_attempts=60
attempt=0
while [ $attempt -lt $max_attempts ]; do
  if curl -sf http://localhost:3000/__opr/health > /dev/null 2>&1; then
    echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[32m MySQL is ready! \e[0m"

    # Record startup timing
    end=$(awk '{print $1}' /proc/uptime)
    elapsed=$(awk "BEGIN{printf \"%.3f\", $end - $start}")
    echo "startup=$elapsed" >> /mnt/telemetry/timings.txt

    # Keep the container running
    wait
    exit 0
  fi

  echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m MySQL not ready yet (attempt $attempt/$max_attempts)... \e[0m"
  sleep 2
  attempt=$((attempt + 1))
done

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[91m MySQL failed to start within timeout \e[0m"
exit 1
