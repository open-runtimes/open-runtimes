#!/bin/bash
# Fail if any command fails
set -e

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[32m Starting MongoDB Runtime... \e[0m"

# Ensure telemetry directory exists
mkdir -p /mnt/telemetry
mkdir -p /mnt/logs

# Record startup time and pass to server
start=$(awk '{print $1}' /proc/uptime)

# Start the OpenRuntimes management server (which starts MongoDB)
cd /usr/local/server
STARTUP_TIME=$start node src/server.js &

# Wait for MongoDB to be ready
echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m Waiting for MongoDB to be ready... \e[0m"

max_attempts=60
attempt=0
while [ $attempt -lt $max_attempts ]; do
  if curl -sf http://localhost:3000/__opr/health > /dev/null 2>&1; then
    echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[32m MongoDB is ready! \e[0m"

    # Keep the container running
    wait
    exit 0
  fi

  echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m MongoDB not ready yet (attempt $attempt/$max_attempts)... \e[0m"
  sleep 2
  attempt=$((attempt + 1))
done

echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[91m MongoDB failed to start within timeout \e[0m"
exit 1
