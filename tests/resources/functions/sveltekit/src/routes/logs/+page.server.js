/** @type {import('./$types').PageLoad} */
export function load() {
	console.log("A log printed");
    console.error("An error printed");

	return {
		msg: "All logs printed"
	};
}