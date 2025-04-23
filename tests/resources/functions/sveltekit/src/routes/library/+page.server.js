import { faker } from '@faker-js/faker';

/** @type {import('./$types').PageLoad} */
export function load() {
    const id = faker.string.uuid()

	return {
		msg: "My UUID is: " + id
	};
}