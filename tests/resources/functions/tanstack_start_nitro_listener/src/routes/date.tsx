import { createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/date")({
  component: RouteComponent,
  loader: async () => {
    return {
      date: new Date().toISOString(),
    };
  },
});

function RouteComponent() {
  const data = Route.useLoaderData();

  return <p>[DATE_START]{data.date}[DATE_END]</p>;
}
