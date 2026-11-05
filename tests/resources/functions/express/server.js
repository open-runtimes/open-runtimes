const express = require("express");

const app = express();

app.use(express.json());

app.get("/", (req, res) => {
  res.send("Hello Open Runtimes");
});

app.get("/users/:id", (req, res) => {
  res.json({ id: req.params.id, sort: req.query.sort });
});

app.post("/echo", (req, res) => {
  res.status(201).json(req.body);
});

app.get("/env", (req, res) => {
  res.send(process.env.CUSTOM_ENV_VAR ?? "");
});

app.get("/logs", (req, res) => {
  console.log("A log printed");
  console.error("An error printed");
  res.send("All logs printed");
});

app.get("/exception", () => {
  throw new Error("Code exception occurred");
});

app.get("/slow", (req, res) => {
  setTimeout(() => res.send("Slow response"), 3000);
});

app.listen(process.env.PORT, () => {
  console.log(`Express server started on http://0.0.0.0:${process.env.PORT}`);
});
