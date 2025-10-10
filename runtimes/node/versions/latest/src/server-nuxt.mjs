import { listener } from "./server/index.mjs";
import express from "express";
import { onError } from "./ssr/helpers.mjs";

console.log("Nuxt server starting ...");

const app = express();

// framework-specific logic
app.use(express.static("public"));
app.use(listener);
// End of framework-specific logic

app.use(onError);

app.listen(+(process.env.PORT || "3000"), process.env.HOST || "0.0.0.0", () => {
  console.log(`Nuxt server started.`);
});
