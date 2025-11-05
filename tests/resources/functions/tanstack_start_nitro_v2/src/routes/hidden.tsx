import { createFileRoute } from "@tanstack/react-router";
import json from "./.config/.file.json";

export const Route = createFileRoute("/hidden")({
  component: RouteComponent,
  loader: async () => {
    return {
      value: json.value,
    };
  },
});

function RouteComponent() {
  const data = Route.useLoaderData();

  return <p>{data.value}</p>;
}
