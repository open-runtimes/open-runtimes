#!/bin/bash
if [[ "${OPEN_RUNTIMES_NFT:-}" != "enabled" ]] && [[ "${OPEN_RUNTIMES_MODCLEAN,,}" != "disabled" ]]; then
	modclean --patterns default:safe --no-progress --run
fi
