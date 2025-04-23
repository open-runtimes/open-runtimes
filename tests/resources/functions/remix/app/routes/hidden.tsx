import { useLoaderData } from "@remix-run/react";
import json from './.config/.file.json';

export async function loader() {
  const value = json.value;
  return { value };
}

export default function Index() {
  const data = useLoaderData();
  const { value } = data;

  return (
    <p>{ value }</p>
  );
}
