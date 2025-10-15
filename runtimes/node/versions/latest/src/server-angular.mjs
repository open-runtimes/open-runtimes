import { reqHandler } from "./server/server.mjs";
import express from "express";

console.log("Angular server starting ...");

const app = express();

// framework-specific logic
app.use(onAction(reqHandler));
// End of framework-specific logic

const port = +(process.env.PORT || '3000');
const host = process.env.HOST || '0.0.0.0';
app.listen(port, host, () => {
  console.log(`Angular server started on http://${host}:${port}`);
});