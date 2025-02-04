/** @type {import('./$types').PageLoad} */
export function load() {
	throw new Error('Code exception occured');

	return {
		msg: "No exceptions"
	};
}