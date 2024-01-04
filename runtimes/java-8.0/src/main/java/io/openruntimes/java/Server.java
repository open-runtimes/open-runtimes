package io.openruntimes.java;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import org.rapidoid.http.Req;
import org.rapidoid.http.Resp;
import org.rapidoid.setup.On;

import java.io.UnsupportedEncodingException;
import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.*;
import java.util.ArrayList;
import java.util.List;

public class Server {
    private static final Gson gson = new GsonBuilder().serializeNulls().create();

    private static final ExecutorService executor = Executors.newCachedThreadPool();

    public static void main(String[] args) {
        On.port(3000);
        On.get("/*").plain(Server::execute);
        On.post("/*").plain(Server::execute);
        On.put("/*").plain(Server::execute);
        On.delete("/*").plain(Server::execute);
        On.patch("/*").plain(Server::execute);
        On.options("/*").plain(Server::execute);
        On.head("/*").plain(Server::execute);
    }

    public static Resp execute(Req req, Resp resp) {
        try {
            return Server.action(req, resp);
        } catch (Exception e) {
            List<String> logs = new ArrayList<>();
            List<String> errors = new ArrayList<>();

            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            e.printStackTrace(pw);

            errors.add(sw.toString());

            resp = resp.header("x-open-runtimes-logs", URLEncoder.encode(String.join("\n", logs), StandardCharsets.UTF_8.toString()));
            resp = resp.header("x-open-runtimes-errors", URLEncoder.encode(String.join("\n", errors), StandardCharsets.UTF_8.toString()));

            return resp
                .code(500)
                .result("");
        }
    }

    public static Resp action(Req req, Resp resp) {
        Map<String, String> reqHeaders = req.headers();

        ArrayList<String> cookieHeaders = new ArrayList<String>();

        for (Map.Entry<String, String> entry : req.cookies().entrySet()) {
            String key = entry.getKey();
            String value = entry.getValue();
            cookieHeaders.add(key + "=" + value);
        }

        if (!(cookieHeaders.isEmpty())) {
            reqHeaders.put("cookie", String.join("; ", cookieHeaders));
        }

        int safeTimeout = -1;
        String timeout = reqHeaders.get("x-open-runtimes-timeout");
        if (timeout != null && !timeout.isEmpty()) {
            boolean invalid = false;

            try {
                safeTimeout = Integer.parseInt(timeout);
            } catch (NumberFormatException e) {
                invalid = true;
            }

            if (invalid || safeTimeout == 0) {
                return resp.code(500).result("Header \"x-open-runtimes-timeout\" must be an integer greater than 0.");
            }
        }

        String serverSecret = System.getenv("OPEN_RUNTIMES_SECRET");
        if (serverSecret == null) {
            serverSecret = "";
        }

        if (reqHeaders.getOrDefault("x-open-runtimes-secret", "").equals("") || !reqHeaders.getOrDefault("x-open-runtimes-secret", "").equals(serverSecret)) {
            return resp.code(500).result("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.");
        }

        String bodyRaw = req.body() == null ? "" : new String(req.body(), StandardCharsets.UTF_8);
        Object body = bodyRaw;
        Map<String, String> headers = new HashMap<>();
        String method = req.verb();

        for (Map.Entry<String, String> entry : reqHeaders.entrySet()) {
            String header = entry.getKey().toLowerCase();
            if (!(header.startsWith("x-open-runtimes-"))) {
                headers.put(header, entry.getValue());
            }
        }

        String contentType = reqHeaders.getOrDefault("content-type", "text/plain");
        if (contentType.contains("application/json")) {
            if (!bodyRaw.isEmpty()) {
                body = gson.fromJson(bodyRaw, Map.class);
            } else {
                body = new HashMap<String, Object>();
            }
        }

        String scheme = reqHeaders.getOrDefault("x-forwarded-proto", "http");
        String defaultPort = scheme.equals("https") ? "443" : "80";

        String hostHeader = reqHeaders.getOrDefault("host", "");
        String host = "";
        int port = Integer.parseInt(defaultPort);

        if (hostHeader.contains(":")) {
            host = hostHeader.split(":")[0];
            port = Integer.parseInt(hostHeader.split(":")[1]);
        } else {
            host = hostHeader;
            port = Integer.parseInt(defaultPort);
        }

        String path = req.path();
        String queryString = req.query();
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
                body,
                bodyRaw,
                url
        );
        RuntimeResponse runtimeResponse = new RuntimeResponse();
        RuntimeContext context = new RuntimeContext(runtimeRequest, runtimeResponse);

        PrintStream systemOut = System.out;
        PrintStream systemErr = System.err;

        ByteArrayOutputStream customStdStream = new ByteArrayOutputStream();
        PrintStream customStd = new PrintStream(customStdStream);
        System.setOut(customStd);
        System.setErr(customStd);

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
            System.out.flush();
            System.err.flush();
            System.setOut(systemOut);
            System.setErr(systemErr);
        }

        if (output == null) {
            context.error("Return statement missing. return context.res.empty() if no response is expected.");
            output = context.getRes().send("", 500);
        }

        for (Map.Entry<String, String> entry : output.getHeaders().entrySet()) {
            String header = entry.getKey().toLowerCase();
            String headerValue = entry.getValue();

            if (header.startsWith("x-open-runtimes-")) {
                continue;
            }

            if (header.equals("content-type") && !headerValue.startsWith("multipart/") && !headerValue.contains("charset=")) {
                headerValue += "; charset=utf-8";
            }

            resp = resp.header(header, headerValue);
        }

        if (!customStdStream.toString().isEmpty()) {
            context.log("");
            context.log("----------------------------------------------------------------------------");
            context.log("Unsupported logs detected. Use context.log() or context.error() for logging.");
            context.log("----------------------------------------------------------------------------");
            context.log(customStdStream.toString());
            context.log("----------------------------------------------------------------------------");
        }

        try {
            resp = resp.header("x-open-runtimes-logs", URLEncoder.encode(String.join("\n", context.getLogs()), StandardCharsets.UTF_8.toString()));
            resp = resp.header("x-open-runtimes-errors", URLEncoder.encode(String.join("\n", context.getErrors()), StandardCharsets.UTF_8.toString()));
        } catch (UnsupportedEncodingException e) {
            context.log("Unsupported encoding detected.");
        }
        return resp
                .code(output.getStatusCode())
                .result(output.getBody());
    }
}