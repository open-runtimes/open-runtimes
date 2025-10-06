import { readFile } from "node:fs/promises";
import express from "express";

const routes = express.Router();

routes.get("/__opr/health", async (_, res) => {
  return res.status(200).send("OK");
});

routes.get("/__opr/timings", async (_, res) => {
  const timings = await readFile("/mnt/telemetry/timings.txt", "utf8");
  res.setHeader("content-type", "text/plain; charset=utf-8");
  return res.status(200).send(timings);
});

export { routes };
