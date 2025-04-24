import { handler } from "./server/index.mjs";
import express from "express";
import { onInit, getPort, getHost, onAction, onError } from "./ssr/helpers.js";
import { Logger } from "./ssr/logger.js";

console.log("Analog server starting ...");

const app = express();
app.use(onInit);

// framework-specific logic
app.use(express.static("public"));
app.use(onAction(handler));
// End of framework-specific logic

app.use(async (req, res, next) => {
  await Logger.end(req.loggerId);
});

app.use(onError);

app.listen(getPort(), getHost(), () => {
  console.log(`Analog server started on http://${getHost()}:${getPort()}`);
});
