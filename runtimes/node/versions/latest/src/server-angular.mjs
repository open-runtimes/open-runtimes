import { reqHandler } from "./server/server.mjs";
import express from "express";
import { onInit, getPort, getHost, onAction, onError, beforeAction, afterAction } from "./ssr/helpers.js";
import { Logger } from "./ssr/logger.js";

console.log("Angular server starting ...");

const app = express();
app.use(onInit);
app.use(beforeAction);
app.use(onAction(reqHandler)); // Framework-specific
app.use(afterAction);
app.use(onError);

app.listen(getPort(), getHost(), () => {
  console.log(`Angular server started on http://${getHost()}:${getPort()}`);
});
