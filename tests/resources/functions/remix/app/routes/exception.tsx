import { useLoaderData } from "@remix-run/react";

export async function loader() {
  throw new Error('Code exception occurred');
  const msg = "No exceptions";
  return { msg };
}

export default function Index() {
  const data = useLoaderData();
  const { msg } = data;

  return (
    <p>{ msg }</p>
  );
}
