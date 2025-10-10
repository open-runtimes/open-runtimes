import express from "express";
import { createRequestHandler } from "@remix-run/express";
import * as build from "./build/server/index.js";

console.log("Remix server starting ...");

const app = express();

// framework-specific logic
app.use(express.static("build/client"));
app.all(
  "*",
  createRequestHandler({
    build,
    getLoadContext(req, res) {
      return {};
    },
  }),
);
// End of framework-specific logic

app.listen(+(process.env.PORT || "3000"), process.env.HOST || "0.0.0.0", () => {
  console.log(`Remix server started.`);
});
