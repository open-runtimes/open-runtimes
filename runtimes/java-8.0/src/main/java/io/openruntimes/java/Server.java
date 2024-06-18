package io.openruntimes.java;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.ToNumberPolicy;
import io.javalin.Javalin;
import io.javalin.http.Context;

import java.io.Console;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.*;

public class Server {
    private static final Gson gsonInternal = new GsonBuilder().serializeNulls().setObjectToNumberStrategy(ToNumberPolicy.LONG_OR_DOUBLE).create();

    private static final ExecutorService executor = Executors.newCachedThreadPool();

    public static void main(String[] args) {

        Javalin
                .create()
                .start(3000)
                .get("/*", Server::execute)
                .post("/*", Server::execute)
                .put("/*", Server::execute)
                .delete("/*", Server::execute)
                .patch("/*", Server::execute)
                .options("/*", Server::execute)
                .head("/*", Server::execute);
    }

    public static Context execute(Context ctx) {
        RuntimeLogger logger = null;

        try {
            logger = new RuntimeLogger(ctx.header("x-open-runtimes-logging"), ctx.header("x-open-runtimes-log-id"));
        } catch (IOException e) {
            System.err.println(e.getMessage());
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
                logger.write(message, RuntimeLogger.TYPE_ERROR, false);
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
                return ctx.status(500).result("Header \"x-open-runtimes-timeout\" must be an integer greater than 0.");
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
            return ctx.status(500).result("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.");
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

        String hostHeader = (hostHeader = ctx.header("host")) != null ? hostHeader : "";;
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

        RuntimeRequest runtimeRequest = new RuntimeRequest(
                method,
                scheme,
                host,
                port,
                path,
                query,
                queryString,
                headers,
                bodyBinary,
                url
        );
        RuntimeResponse runtimeResponse = new RuntimeResponse();
        RuntimeContext context = new RuntimeContext(runtimeRequest, runtimeResponse, logger);

        logger.overrideNativeLogs();

        RuntimeOutput output;

        try {
            String entrypoint = System.getenv("OPEN_RUNTIMES_ENTRYPOINT");
            entrypoint = entrypoint.substring(0, entrypoint.length() - 5); // Remove .java
            entrypoint = entrypoint.replaceAll("/", ".");

            final Class classToLoad = Class.forName("io.openruntimes.java." + entrypoint);
            final Method classMethod = classToLoad.getDeclaredMethod("main", RuntimeContext.class);
            final Object instance = classToLoad.newInstance();

            if (safeTimeout > 0) {
                Future<RuntimeOutput> future = executor.submit(() -> {
                    try {
                        return (RuntimeOutput) classMethod.invoke(instance, context);
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

        if (output == null) {
            context.error("Return statement missing. return context.res.empty() if no response is expected.");
            output = context.getRes().send("", 500);
        }

        output.getHeaders().putIfAbsent("content-type", "text/plain");

        for (Map.Entry<String, String> entry : output.getHeaders().entrySet()) {
            String header = entry.getKey().toLowerCase();
            String headerValue = entry.getValue();

            if (header.startsWith("x-open-runtimes-")) {
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

        try {
            logger.end();
        } catch (IOException e) {
            // Ignore missing logs
        }

        return ctx.status(output.getStatusCode()).result(output.getBody());
    }
}