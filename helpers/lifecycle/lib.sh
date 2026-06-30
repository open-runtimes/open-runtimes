#!/bin/bash
# Shared utilities for the lifecycle runner. Source this first.

OPR_HOOKS_DIR="/usr/local/server/hooks"

opr_log() {
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[97m $* \e[0m"
}

opr_error() {
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[31m $* \e[0m"
}

opr_success() {
	echo -e "\e[90m$(date +[%H:%M:%S]) \e[31m[\e[0mopen-runtimes\e[31m]\e[32m $* \e[0m"
}

opr_uptime() {
	awk '{print $1}' /proc/uptime
}

# Source a runtime's hook if it ships one; missing hook means the phase is a
# no-op. Hooks are sourced (not executed) on purpose: they set environment
# that must persist across phases (python's venv activate, OPEN_RUNTIMES_CLEANUP
# from pack hooks).
opr_run_hook() {
	local hook="$OPR_HOOKS_DIR/$1.sh"
	if [ -f "$hook" ]; then
		# shellcheck disable=SC1090
		. "$hook"
	fi
}
