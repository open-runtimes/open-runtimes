/** @type {import('./$types').PageLoad} */
export async function load() {
	for (let i = 1; i <= 3; i++) {
		console.log("Concurrent Log " + i);
		await new Promise(resolve => setTimeout(resolve, 500 + Math.random() * 500));
	}

	return {
		ok: true
	};
}