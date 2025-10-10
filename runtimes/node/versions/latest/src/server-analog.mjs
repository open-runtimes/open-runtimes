import { handler } from "./server/index.mjs";
import express from "express";
import { onAction, onError } from "./ssr/helpers.mjs";

console.log("Analog server starting ...");

const app = express();

// framework-specific logic
app.use(express.static("public"));
app.use(onAction(handler));
// End of framework-specific logic

app.use(onError);

app.listen(process.env.PORT, process.env.HOST, () => {
  console.log(`Analog server started on http://${getHost()}:${getPort()}`);
});
