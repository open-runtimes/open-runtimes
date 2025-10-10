import { handler } from "./server/entry.mjs";
import express from "express";
import { onAction, onError } from "./ssr/helpers.mjs";

console.log("Astro server starting ...");

const app = express();

// framework-specific logic
app.use(express.static("client"));
app.use(onAction(handler));
// End of framework-specific logic

app.use(onError);

app.listen(process.env.PORT, process.env.HOST, () => {
  console.log(`Astro server started on http://${getHost()}:${getPort()}`);
});
