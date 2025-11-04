import { handler } from "./server/entry.mjs";
import express from "express";

console.log("Astro server starting ...");

const app = express();

// framework-specific logic
app.use(
  express.static("client", {
    setHeaders: (res, path) => {
      res.setHeader(
        process.env.OPEN_RUNTIMES_CACHE_HEADER ?? "CDN-Cache-Control",
        "public, max-age=36000",
      );
    },
  }),
);
app.use(handler);
// End of framework-specific logic

const port = +(process.env.PORT || "3000");
const host = process.env.HOST || "0.0.0.0";
app.listen(port, host, () => {
  console.log(`Astro server started on http://${host}:${port}`);
});
