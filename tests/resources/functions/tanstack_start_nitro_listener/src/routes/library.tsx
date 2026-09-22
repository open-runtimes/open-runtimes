import { createFileRoute } from "@tanstack/react-router";
import { faker } from "@faker-js/faker";

export const Route = createFileRoute("/library")({
  component: RouteComponent,
  loader: async () => {
    const id = faker.string.uuid();
    return {
      msg: "My UUID is: " + id,
    };
  },
});

function RouteComponent() {
  const data = Route.useLoaderData();

  return <p>[UUID_START]{data.msg}[UUID_END]</p>;
}
