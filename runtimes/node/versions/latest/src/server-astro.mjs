import { handler } from "./server/entry.mjs";
import express from "express";

console.log("Astro server starting ...");

const app = express();

// framework-specific logic
app.use(express.static("client"));
app.use(onAction(handler));
// End of framework-specific logic

const port = +(process.env.PORT || '3000');
const host = process.env.HOST || '0.0.0.0';
app.listen(port, host, () => {
  console.log(`Astro server started on http://${host}:${port}`);
});