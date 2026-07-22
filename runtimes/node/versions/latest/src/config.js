let headers = {};
try {
  headers = JSON.parse(process.env.OPEN_RUNTIMES_HEADERS || "{}");
} catch {
  // Silently ignore invalid JSON and keep empty enforced headers
}

module.exports = Object.freeze({
  secret: process.env.OPEN_RUNTIMES_SECRET,
  headers,
  entrypoint: process.env.OPEN_RUNTIMES_ENTRYPOINT,
  env: process.env.OPEN_RUNTIMES_ENV,
});
