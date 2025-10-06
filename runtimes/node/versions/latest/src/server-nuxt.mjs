import { listener } from "./server/index.mjs";
import express from "express";
import { onInit, getPort, getHost, onAction, onError } from "./ssr/helpers.mjs";
import * as system from "./ssr/system.mjs";

console.log("Nuxt server starting ...");

const app = express();

app.use(system.routes);
app.use(onInit);

// framework-specific logic
app.use(express.static("public"));
app.use(onAction(listener));
// End of framework-specific logic

app.use(onError);

app.listen(getPort(), getHost(), () => {
  console.log(`Nuxt server started on http://${getHost()}:${getPort()}`);
});
