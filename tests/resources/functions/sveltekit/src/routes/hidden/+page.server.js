import json from './.config/.file.json';

/** @type {import('./$types').PageLoad} */
export function load() {
	return {
		value: json.value
	};
}