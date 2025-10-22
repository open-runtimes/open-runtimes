/** @type {import('./$types').PageLoad} */
export function load() {
	throw new Error('Code exception occurred');

	return {
		msg: "No exceptions"
	};
}