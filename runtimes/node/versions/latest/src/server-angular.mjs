import { reqHandler } from "./server/server.mjs";
import { onInit, getPort, getHost, onAction, onError } from "./ssr/helpers.mjs";
import express from "express";
import * as system from "./ssr/system.mjs";

console.log("Angular server starting ...");

const app = express();

app.use(system.routes);
app.use(onInit);

// framework-specific logic
app.use(onAction(reqHandler));
// End of framework-specific logic

app.use(onError);

app.listen(getPort(), getHost(), () => {
  console.log(`Angular server started on http://${getHost()}:${getPort()}`);
});
