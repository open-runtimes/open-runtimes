#!/bin/bash

# Telemetry timing script using /proc/uptime
# Usage: ./timings.sh start|stop <phase_name>

STARTS_FILE="/mnt/telemetry/starts.txt"
TIMINGS_FILE="/mnt/telemetry/timings.txt"

# Ensure telemetry directory exists
mkdir -p /mnt/telemetry

# Get current uptime in seconds with microsecond precision
get_uptime() {
    awk '{print $1}' /proc/uptime
}

# Function to start timing a phase
start_phase() {
    local phase="$1"
    local uptime=$(get_uptime)

    # Record start time
    echo "${phase}=${uptime}" >> "$STARTS_FILE"
}

# Function to stop timing a phase and calculate duration
stop_phase() {
    local phase="$1"
    local stop_time=$(get_uptime)

    # Find the most recent start time for this phase
    local start_time=$(grep "^${phase}=" "$STARTS_FILE" 2>/dev/null | tail -1 | cut -d'=' -f2)

    if [ -z "$start_time" ]; then
        echo -e "\033[90m$(date +[%H:%M:%S]) \033[31m[\033[00mopen-telemetry\033[31m]\033[97m Error: No start time found for phase '$phase' \033[0m"
        exit 1
    fi

    # Calculate duration using awk for floating point arithmetic
    local duration=$(awk "BEGIN{printf \"%.3f\", $stop_time - $start_time}")

    # Record timing
    echo "${phase}=${duration}" >> "$TIMINGS_FILE"
    echo -e "\033[90m$(date +[%H:%M:%S]) \033[31m[\033[00mopen-telemetry\033[31m]\033[97m Timed phase '$phase' in ${duration}s \033[0m"
}

# Main script logic
case "$1" in
    start)
        if [ -z "$2" ]; then
            echo -e "\033[90m$(date +[%H:%M:%S]) \033[31m[\033[00mopen-telemetry\033[31m]\033[97m Usage: $0 start <phase_name> \033[0m"
            exit 1
        fi
        start_phase "$2"
        ;;
    stop)
        if [ -z "$2" ]; then
            echo -e "\033[90m$(date +[%H:%M:%S]) \033[31m[\033[00mopen-telemetry\033[31m]\033[97m Usage: $0 stop <phase_name> \033[0m"
            exit 1
        fi
        stop_phase "$2"
        ;;
    *)
        echo -e "\033[90m$(date +[%H:%M:%S]) \033[31m[\033[00mtelemetry\033[31m]\033[97m Usage: $0 start|stop <phase_name> \033[0m"
        echo -e "\033[90m$(date +[%H:%M:%S]) \033[31m[\033[00mtelemetry\033[31m]\033[97m Examples: \033[0m"
        echo -e "\033[90m$(date +[%H:%M:%S]) \033[31m[\033[00mtelemetry\033[31m]\033[97m   $0 start boot \033[0m"
        echo -e "\033[90m$(date +[%H:%M:%S]) \033[31m[\033[00mtelemetry\033[31m]\033[97m   $0 stop boot \033[0m"
        echo -e "\033[90m$(date +[%H:%M:%S]) \033[31m[\033[00mtelemetry\033[31m]\033[97m   $0 start network_init \033[0m"
        echo -e "\033[90m$(date +[%H:%M:%S]) \033[31m[\033[00mtelemetry\033[31m]\033[97m   $0 stop network_init \033[0m"
        exit 1
        ;;
esac
