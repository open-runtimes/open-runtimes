import { Buffer } from "node:buffer";

// When edge asks for logs inline, the body goes out in `b` frames and the logs
// and errors follow in `l` and `e` frames, so edge splits them off a body it
// streams. Each frame is its type, the payload's byte length in hex and a
// newline, then the payload.
const frame = (type, bytes) =>
  Buffer.concat([Buffer.from(`${type}${bytes.length.toString(16)}\n`), bytes]);

const toBytes = (chunk, encoding) =>
  Buffer.isBuffer(chunk)
    ? chunk
    : typeof chunk === "string"
      ? Buffer.from(chunk, typeof encoding === "string" ? encoding : "utf8")
      : Buffer.from(chunk.buffer, chunk.byteOffset, chunk.byteLength);

// Frames everything the framework writes to `res`, then appends the request's
// logs once it ends. `logs()` returns the [logs, errors] buffers.
export function frameResponse(res, logs) {
  const { writeHead, write, end } = res;
  let started = false;

  // Framing gives the body a length of its own and needs a status that allows
  // a body, so the real ones travel in headers
  const start = () => {
    if (started) {
      return;
    }
    started = true;

    const length = res.getHeader("content-length");
    res.removeHeader("content-length");
    if (length !== undefined) {
      res.setHeader("x-open-runtimes-content-length", length);
    }
    if (res.statusCode === 204 || res.statusCode === 304) {
      res.setHeader("x-open-runtimes-status-code", res.statusCode);
      res.statusCode = 200;
    }
    res.setHeader("x-open-runtimes-logs-inline", "enabled");
  };

  // Headers passed here would skip start(), so they are set on `res` first
  res.writeHead = function (statusCode, reason, headers) {
    if (typeof reason !== "string") {
      headers = reason;
      reason = undefined;
    }
    res.statusCode = statusCode;
    if (Array.isArray(headers)) {
      const pairs = Array.isArray(headers[0])
        ? headers
        : headers.flatMap((value, i) =>
            i % 2 === 0 ? [[value, headers[i + 1]]] : [],
          );
      const values = {};
      for (const [key, value] of pairs) {
        (values[key.toLowerCase()] ??= []).push(value);
      }
      for (const key in values) {
        res.setHeader(
          key,
          values[key].length === 1 ? values[key][0] : values[key],
        );
      }
    } else if (headers) {
      for (const key in headers) {
        res.setHeader(key, headers[key]);
      }
    }
    start();
    return reason === undefined
      ? writeHead.call(res, res.statusCode)
      : writeHead.call(res, res.statusCode, reason);
  };

  res.write = function (chunk, encoding, callback) {
    start();
    const bytes = toBytes(chunk, encoding);
    callback = typeof encoding === "function" ? encoding : callback;
    if (bytes.length === 0) {
      callback?.();
      return true;
    }
    return write.call(res, frame("b", bytes), callback);
  };

  res.end = function (chunk, encoding, callback) {
    if (typeof chunk === "function") {
      callback = chunk;
      chunk = undefined;
    } else if (typeof encoding === "function") {
      callback = encoding;
    }
    const bytes = chunk == null ? Buffer.alloc(0) : toBytes(chunk, encoding);

    // A body sent whole has the length Node would have given it
    if (!started && !res.headersSent && !res.hasHeader("content-length")) {
      res.setHeader("content-length", bytes.length);
    }
    start();

    if (bytes.length > 0) {
      write.call(res, frame("b", bytes));
    }
    const [logsBuffer, errorsBuffer] = logs();
    if (logsBuffer.length > 0) {
      write.call(res, frame("l", logsBuffer));
    }
    if (errorsBuffer.length > 0) {
      write.call(res, frame("e", errorsBuffer));
    }
    return end.call(res, callback);
  };
}
