import { handler } from "./handler.js";
import express from "express";
import { onAction, onError } from "./ssr/helpers.mjs";

console.log("SvelteKit server starting ...");

const app = express();

// framework-specific logic
app.use(onAction(handler));
// End of framework-specific logic

app.use(onError);

app.listen(process.env.PORT, process.env.HOST, () => {
  console.log(`SvelteKit server started on http://${getHost()}:${getPort()}`);
});
