import { createFileRoute } from "@tanstack/react-router";

export const Route = createFileRoute("/exception")({
  component: RouteComponent,
  loader: async () => {
    throw new Error("Code exception occured");

    return {
      msg: "No exceptions",
    };
  },
});

function RouteComponent() {
  const data = Route.useLoaderData();

  return <p>{data.msg}</p>;
}
