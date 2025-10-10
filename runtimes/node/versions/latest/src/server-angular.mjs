import { reqHandler } from "./server/server.mjs";
import express from "express";

console.log("Angular server starting ...");

const app = express();

// framework-specific logic
app.use(reqHandler);
// End of framework-specific logic

app.listen(+(process.env.PORT || "3000"), process.env.HOST || "0.0.0.0", () => {
  console.log(`Angular server started.`);
});
