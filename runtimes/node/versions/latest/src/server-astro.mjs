import { handler } from "./server/entry.mjs";
import express from "express";
import { onInit, getPort, getHost, onAction, onError } from "./ssr/helpers.mjs";
import * as system from "./ssr/system.mjs";

console.log("Astro server starting ...");

const app = express();

app.use(system.routes);
app.use(onInit);

// framework-specific logic
app.use(express.static("client"));
app.use(onAction(handler));
// End of framework-specific logic

app.use(onError);

app.listen(getPort(), getHost(), () => {
  console.log(`Astro server started on http://${getHost()}:${getPort()}`);
});
