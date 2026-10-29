package io.openruntimes.java;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.ToNumberPolicy;
import io.javalin.Javalin;
import io.javalin.http.Context;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.*;
import org.eclipse.jetty.server.Request;

public class Server {
  private static final Gson gsonInternal =
      new GsonBuilder()
          .serializeNulls()
          .setObjectToNumberStrategy(ToNumberPolicy.LONG_OR_DOUBLE)
          .create();

  private static final ExecutorService executor = Executors.newCachedThreadPool();

  public static void main(String[] args) {
    System.out.println("HTTP server successfully started!");

    Javalin app =
        Javalin.create(
                config -> {
                  config.maxRequestSize = 20L * 1024 * 1024;
                })
            .start(3000)
            .get("/*", Server::execute)
            .post("/*", Server::execute)
            .put("/*", Server::execute)
            .delete("/*", Server::execute)
            .patch("/*", Server::execute)
            .options("/*", Server::execute)
            .head("/*", Server::execute);
    app.jettyServer().server().setStopTimeout(30_000);
    Runtime.getRuntime()
        .addShutdownHook(
            new Thread(
                () -> {
                  app.stop();
                  executor.shutdown();
                }));
  }

  public static Context execute(Context ctx) {
    if (ctx.path().equals("/__opr/health")) {
      return ctx.status(200).result("OK");
    }
    if (ctx.path().equals("/__opr/timings")) {
      try {
        String timings = new String(Files.readAllBytes(Paths.get("/mnt/telemetry/timings.txt")));
        return ctx.contentType("text/plain; charset=utf-8").result(timings);
      } catch (IOException e) {
        return ctx.status(500).result("Error reading timings");
      }
    }

    RuntimeLogger logger = null;

    try {
      logger =
          new RuntimeLogger(
              ctx.header("x-open-runtimes-logging"), ctx.header("x-open-runtimes-log-id"));
    } catch (IOException e) {
      // Ignore missing logs
      try {
        logger = new RuntimeLogger("disabled", "");
      } catch (IOException e2) {
        // Never happens
      }
    }

    try {
      return Server.action(logger, ctx);
    } catch (Exception e) {
      StringWriter sw = new StringWriter();
      PrintWriter pw = new PrintWriter(sw);
      e.printStackTrace(pw);
      String message = sw.toString();

      ctx.header("x-open-runtimes-log-id", logger.getId());

      try {
        String[] logs = new String[1];
        logs[0] = message.toString();
        logger.write(logs, RuntimeLogger.TYPE_ERROR, false);
        logger.end();
      } catch (IOException e2) {
        // Ignore missing logs
      }

      return ctx.status(500).result("");
    }
  }

  public static Context action(RuntimeLogger logger, Context ctx) {
    int safeTimeout = -1;
    String timeout = ctx.header("x-open-runtimes-timeout");
    if (timeout != null && !timeout.isEmpty()) {
      boolean invalid = false;

      try {
        safeTimeout = Integer.parseInt(timeout);
      } catch (NumberFormatException e) {
        invalid = true;
      }

      if (invalid || safeTimeout == 0) {
        return ctx.status(500)
            .result("Header \"x-open-runtimes-timeout\" must be an integer greater than 0.");
      }
    }

    String serverSecret = System.getenv("OPEN_RUNTIMES_SECRET");
    if (serverSecret == null) {
      serverSecret = "";
    }

    String secret = ctx.header("x-open-runtimes-secret");

    if (secret == null) {
      secret = "";
    }

    if (!serverSecret.equals("") && !secret.equals(serverSecret)) {
      return ctx.status(500)
          .result("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.");
    }
    byte[] bodyBinary = ctx.bodyAsBytes();

    Map<String, String> headers = new HashMap<>();
    String method = ctx.method().toString();

    for (Map.Entry<String, String> entry : ctx.headerMap().entrySet()) {
      String header = entry.getKey().toLowerCase();
      if (!(header.startsWith("x-open-runtimes-"))) {
        headers.put(header, entry.getValue());
      }
    }

    String enforcedHeadersString = System.getenv("OPEN_RUNTIMES_HEADERS");

    if (enforcedHeadersString == null || enforcedHeadersString.isEmpty()) {
      enforcedHeadersString = "{}";
    }

    Map<String, Object> enforcedHeaders = gsonInternal.fromJson(enforcedHeadersString, Map.class);

    for (Map.Entry<String, Object> entry : enforcedHeaders.entrySet()) {
      headers.put(entry.getKey().toLowerCase(), String.valueOf(entry.getValue()));
    }

    String scheme = (scheme = ctx.header("x-forwarded-proto")) != null ? scheme : "http";
    String defaultPort = scheme.equals("https") ? "443" : "80";

    String hostHeader = (hostHeader = ctx.header("host")) != null ? hostHeader : "";
    ;
    String host = "";
    int port;

    if (hostHeader.contains(":")) {
      host = hostHeader.split(":")[0];
      port = Integer.parseInt(hostHeader.split(":")[1]);
    } else {
      host = hostHeader;
      port = Integer.parseInt(defaultPort);
    }

    String path = ctx.path();
    String queryString = (queryString = ctx.queryString()) != null ? queryString : "";
    Map<String, String> query = new HashMap<>();

    for (String param : queryString.split("&")) {
      String[] pair = param.split("=", 2);

      if (pair.length >= 1 && pair[0] != null && !pair[0].isEmpty()) {
        String value = pair.length == 2 ? pair[1] : "";
        query.put(pair[0], value);
      }
    }

    String url = scheme + "://" + host;

    if (port != Integer.parseInt(defaultPort)) {
      url += ":" + port;
    }

    url += path;

    if (!queryString.isEmpty()) {
      url += "?" + queryString;
    }

    RuntimeRequest runtimeRequest =
        new RuntimeRequest(
            method, scheme, host, port, path, query, queryString, headers, bodyBinary, url);
    RuntimeResponse runtimeResponse = new RuntimeResponse();
    RuntimeContext context = new RuntimeContext(runtimeRequest, runtimeResponse, logger);

    logger.overrideNativeLogs();

    RuntimeOutput output = null;
    Method classMethod = null;
    Object instance = null;

    String entrypoint = System.getenv("OPEN_RUNTIMES_ENTRYPOINT");

    // Guard: Try to load module
    try {
      String className = entrypoint.substring(0, entrypoint.length() - 5); // Remove .java
      className = className.replaceAll("/", ".");

      final Class classToLoad = Class.forName("io.openruntimes.java." + className);
      classMethod = classToLoad.getDeclaredMethod("main", RuntimeContext.class);
      instance = classToLoad.newInstance();
    } catch (ClassNotFoundException e) {
      context.error("Class not found: " + e.getMessage());
      logger.revertNativeLogs();
      output = context.getRes().send("", 503);
    } catch (NoSuchMethodException e) {
      context.error("Function signature invalid. Did you forget to export a 'main' function?");
      logger.revertNativeLogs();
      output = context.getRes().send("", 503);
    } catch (InstantiationException e) {
      context.error("Failed to create instance: " + e.getMessage());
      logger.revertNativeLogs();
      output = context.getRes().send("", 503);
    } catch (IllegalAccessException e) {
      context.error("Access denied: " + e.getMessage());
      logger.revertNativeLogs();
      output = context.getRes().send("", 503);
    } catch (Exception e) {
      context.error("Failed to load module: " + e.getMessage());
      logger.revertNativeLogs();
      output = context.getRes().send("", 503);
    }

    // Execute user function
    if (output == null && classMethod != null && instance != null) {
      final Method finalClassMethod = classMethod;
      final Object finalInstance = instance;

      try {
        if (safeTimeout > 0) {
          Future<RuntimeOutput> future =
              executor.submit(
                  () -> {
                    try {
                      return (RuntimeOutput) finalClassMethod.invoke(finalInstance, context);
                    } catch (Exception e) {
                      StringWriter sw = new StringWriter();
                      PrintWriter pw = new PrintWriter(sw);
                      e.printStackTrace(pw);

                      context.error(sw.toString());
                      context.getRes().send("", 500);
                    }

                    return null;
                  });

          try {
            output = future.get(safeTimeout, TimeUnit.SECONDS);
          } catch (TimeoutException e) {
            future.cancel(true);
            context.error("Execution timed out.");
            output = context.getRes().send("", 500);
          }
        } else {
          output = (RuntimeOutput) classMethod.invoke(instance, context);
        }
      } catch (Exception e) {
        StringWriter sw = new StringWriter();
        PrintWriter pw = new PrintWriter(sw);
        e.printStackTrace(pw);

        context.error(sw.toString());
        output = context.getRes().send("", 500);
      } finally {
        logger.revertNativeLogs();
      }
    }

    if (output == null) {
      context.error(
          "Return statement missing. return context.res.empty() if no response is expected.");
      output = context.getRes().send("", 500);
    }

    output.getHeaders().putIfAbsent("content-type", "text/plain");

    for (Map.Entry<String, String> entry : output.getHeaders().entrySet()) {
      String header = entry.getKey().toLowerCase();
      String headerValue = entry.getValue();

      if (header.startsWith("x-open-runtimes-")) {
        continue;
      }

      // A streamed response is chunked, so its length is unknown.
      if (output.getStream() != null && header.equals("content-length")) {
        continue;
      }

      if (header.equals("content-type") && !headerValue.startsWith("multipart/")) {
        headerValue = headerValue.toLowerCase();

        if (!headerValue.contains("charset=")) {
          headerValue += "; charset=utf-8";
        }
      }

      ctx.header(header, headerValue);
    }
    ctx.header("x-open-runtimes-log-id", logger.getId());

    if (output.getStream() != null) {
      // Status and headers leave with the first chunk, so they are final from here on.
      ctx.status(output.getStatusCode());
      stream(ctx, output.getStream(), context, logger, safeTimeout);
      return ctx;
    }

    try {
      logger.end();
    } catch (IOException e) {
      // Ignore missing logs
    }

    return ctx.status(output.getStatusCode()).result(output.getBody());
  }

  private static void stream(
      Context ctx,
      Iterable<?> source,
      RuntimeContext context,
      RuntimeLogger logger,
      int safeTimeout) {
    Callable<Void> write =
        () -> {
          OutputStream out = ctx.res.getOutputStream();
          for (Object chunk : source) {
            byte[] bytes =
                chunk instanceof byte[]
                    ? (byte[]) chunk
                    : String.valueOf(chunk).getBytes(StandardCharsets.UTF_8);
            if (bytes.length > 0) {
              out.write(bytes);
              out.flush();
            }
          }
          return null;
        };

    String failure = null;
    logger.overrideNativeLogs();
    try {
      if (safeTimeout > 0) {
        Future<Void> future = executor.submit(write);
        try {
          future.get(safeTimeout, TimeUnit.SECONDS);
        } catch (TimeoutException e) {
          future.cancel(true);
          failure = "Execution timed out.";
        }
      } else {
        write.call();
      }
    } catch (Exception e) {
      Throwable cause = e instanceof ExecutionException ? e.getCause() : e;
      StringWriter sw = new StringWriter();
      PrintWriter pw = new PrintWriter(sw);
      cause.printStackTrace(pw);
      failure = sw.toString();
    } finally {
      logger.revertNativeLogs();
    }

    if (failure != null) {
      context.error(failure);
    }

    // Logs are read once the response finishes, so they are flushed first.
    try {
      logger.end();
    } catch (IOException e) {
      // Ignore missing logs
    }

    // Content is already on the wire, so a failure can only reach the caller as a truncation.
    if (failure != null) {
      Request.getBaseRequest(ctx.req).getHttpChannel().abort(new IOException(failure));
    }
  }
}
