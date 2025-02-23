import { useLoaderData } from "@remix-run/react";

export async function loader() {
  for (let i = 1; i <= 3; i++) {
    console.log("Concurrent Log " + i);
    await new Promise(resolve => setTimeout(resolve, 500 + Math.random() * 500));
  }
  
  return { ok: true };
}

export default function Index() {
  const data = useLoaderData();
  const { ok } = data;

  return (
    <p>{ ok ? 'OK Response' : 'Fail' }</p>
  );
}
