import { handler } from "./handler.js";
import express from "express";
import { onInit, getPort, getHost, onAction, onError } from "./ssr/helpers.js";
import { Logger } from "./ssr/logger.js";

console.log("SvelteKit server starting ...");

const app = express();
app.use(onInit);

// framework-specific logic
app.use(onAction(handler));
// End of framework-specific logic

app.use(async (req, res, next) => {
  await Logger.end(req.loggerId);
});

app.use(onError);

app.listen(getPort(), getHost(), () => {
  console.log(`SvelteKit server started on http://${getHost()}:${getPort()}`);
});
