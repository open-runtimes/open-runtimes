import { reqHandler } from "./server/server.mjs";
import { onAction, onError } from "./ssr/helpers.mjs";
import express from "express";

console.log("Angular server starting ...");

const app = express();

// framework-specific logic
app.use(onAction(reqHandler));
// End of framework-specific logic

app.use(onError);

app.listen(process.env.PORT, process.env.HOST, () => {
  console.log(`Angular server started on http://${getHost()}:${getPort()}`);
});
