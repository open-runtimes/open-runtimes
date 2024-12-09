/** @type {import('./$types').PageLoad} */
export function load() {
	return {
		date: new Date().toISOString()
	};
}