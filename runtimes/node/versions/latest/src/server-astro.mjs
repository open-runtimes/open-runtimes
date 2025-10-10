import { handler } from "./server/entry.mjs";
import express from "express";

console.log("Astro server starting ...");

const app = express();

// framework-specific logic
app.use(express.static("client"));
app.use(handler);
// End of framework-specific logic

app.listen(+(process.env.PORT || "3000"), process.env.HOST || "0.0.0.0", () => {
  console.log(`Astro server started.`);
});
