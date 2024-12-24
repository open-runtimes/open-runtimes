import { useLoaderData } from "@remix-run/react";

export async function loader() {
  console.log("A log printed");
  console.error("An error printed");

  const msg = "All logs printed";
  return { msg };
}

export default function Index() {
  const data = useLoaderData();
  const { msg } = data;

  return (
    <p>{ msg }</p>
  );
}
