import { reqHandler } from "./server/server.mjs";
import express from "express";
import { onInit, getPort, getHost, onAction, onError } from "./helpers.js";

console.log("Angular server starting ...");

const app = express();
app.use(onInit);

// framework-specific logic
app.use(onAction(reqHandler));
// End of framework-specific logic

app.use(onError);

app.listen(getPort(), getHost(), () => {
  console.log(`Angular server started on http://${getHost()}:${getPort()}`);
});
