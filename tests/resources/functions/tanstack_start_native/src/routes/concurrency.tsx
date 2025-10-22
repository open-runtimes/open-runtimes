import { createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/concurrency")({
  component: RouteComponent,
  loader: async () => {
    for (let i = 1; i <= 3; i++) {
      console.log("Concurrent Log " + i);
      await new Promise((resolve) =>
        setTimeout(resolve, 500 + Math.random() * 500),
      );
    }

    return {
      ok: true,
    };
  },
});

function RouteComponent() {
  const data = Route.useLoaderData();

  return <p>{data.ok ? "OK Response" : "Failed"}</p>;
}
