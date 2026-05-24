import superjson from "https://esm.sh/superjson@2.2.6";

export function JSONParse(message: any) {
  return JSON.stringify(superjson.serialize(message).json);
}
