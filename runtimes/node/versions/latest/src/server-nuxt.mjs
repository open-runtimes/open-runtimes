import { listener } from "./server/index.mjs";
import express from "express";
import { onAction, onError } from "./ssr/helpers.mjs";

console.log("Nuxt server starting ...");

const app = express();

// framework-specific logic
app.use(express.static("public"));
app.use(onAction(listener));
// End of framework-specific logic

app.use(onError);

app.listen(process.env.PORT, process.env.HOST, () => {
  console.log(`Nuxt server started on http://${getHost()}:${getPort()}`);
});
