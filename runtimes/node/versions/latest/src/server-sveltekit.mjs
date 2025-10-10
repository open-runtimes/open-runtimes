import { handler } from "./handler.js";
import express from "express";
import { onError } from "./ssr/helpers.mjs";

console.log("SvelteKit server starting ...");

const app = express();

// framework-specific logic
app.use(handler);
// End of framework-specific logic

app.use(onError);

app.listen(+(process.env.PORT || "3000"), process.env.HOST || "0.0.0.0", () => {
  console.log(`SvelteKit server started.`);
});
