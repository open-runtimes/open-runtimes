import { useLoaderData } from "@remix-run/react";
import { faker } from '@faker-js/faker';

export async function loader() {
  const id = faker.string.uuid();
  return {
    msg: "My UUID is: " + id
  };
}

export default function Index() {
  const data = useLoaderData();
  const { msg } = data;

  return (
    <p>[UUID_START]{ msg }[UUID_END]</p>
  );
}
