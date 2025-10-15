import { handler } from "./handler.js";
import express from "express";

console.log("SvelteKit server starting ...");

const app = express();

// framework-specific logic
app.use(handler);
// End of framework-specific logic

const port = +(process.env.PORT || "3000");
const host = process.env.HOST || "0.0.0.0";
app.listen(port, host, () => {
  console.log(`SvelteKit server started on http://${host}:${port}`);
});
