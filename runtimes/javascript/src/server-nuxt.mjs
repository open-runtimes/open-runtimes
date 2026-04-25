import { listener } from "./server/index.mjs";
import express from "express";

console.log("Nuxt server starting ...");

const app = express();

// framework-specific logic
app.use(
  express.static("public", {
    setHeaders: (res, _path) => {
      res.setHeader(
        process.env.OPEN_RUNTIMES_CACHE_HEADER ?? "CDN-Cache-Control",
        "public, max-age=36000",
      );
    },
  }),
);
app.use(listener);
// End of framework-specific logic

const port = +(process.env.PORT || "3000");
const host = process.env.HOST || "0.0.0.0";
app.listen(port, host, () => {
  console.log(`Nuxt server started on http://${host}:${port}`);
});
