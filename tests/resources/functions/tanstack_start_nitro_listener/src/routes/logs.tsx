import { createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/logs")({
  component: RouteComponent,
  loader: async () => {
    console.log("A log printed");
    console.error("An error printed");

    return {
      msg: "All logs printed",
    };
  },
});

function RouteComponent() {
  const data = Route.useLoaderData();

  return <p>{data.msg}</p>;
}
