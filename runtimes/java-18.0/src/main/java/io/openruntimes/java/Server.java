package io.openruntimes.java;

import org.rapidoid.http.Req;
import org.rapidoid.http.Resp;
import org.rapidoid.setup.On;
import java.io.ByteArrayOutputStream;
import java.io.StringWriter;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.util.HashMap;
import java.util.Map;
import com.google.gson.GsonBuilder;
import com.google.gson.Gson;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.io.UnsupportedEncodingException;
import java.lang.reflect.Method;

public class Server {

    public static void main(String[] args) {
        On.port(3000);
        On.get("/*").plain(Server::execute);
        On.post("/*").plain(Server::execute);
        On.put("/*").plain(Server::execute);
        On.delete("/*").plain(Server::execute);
        On.patch("/*").plain(Server::execute);
        On.options("/*").plain(Server::execute);
        On.head("/*").plain(Server::execute);
        On.trace("/*").plain(Server::execute);
    }

    public static Resp execute(Req req, Resp resp) {
        Map<String, String> reqHeaders = req.headers();

        String serverSecret = System.getenv("OPEN_RUNTIMES_SECRET");
        if(serverSecret == null) {
            serverSecret = "";
        }

        if(!reqHeaders.getOrDefault("x-open-runtimes-secret", "").equals(serverSecret)) {
            return resp.code(500).result("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.");
        }

        String bodyString = req.body() == null ? "" : new String(req.body(), StandardCharsets.UTF_8);
        Object body = bodyString;
        Map<String, String> headers = new HashMap<String, String>();
        String method = req.verb();

        for (Map.Entry<String, String> entry : reqHeaders.entrySet()) {
            String header = entry.getKey().toLowerCase();
            if(!(header.startsWith("x-open-runtimes-"))) {
                headers.put(header, entry.getValue());
            }
	    }

        String contentType = reqHeaders.getOrDefault("content-type", "text/plain");
        if(contentType.contains("application/json")) {
            if(!bodyString.isEmpty()) {
                Gson gson = new GsonBuilder().serializeNulls().create();
                body = gson.fromJson(bodyString, Map.class);
            } else {
                body = new HashMap<String, Object>();
            }
        }

        String scheme = reqHeaders.getOrDefault("x-forwarded-proto", "http");

        String hostHeader = reqHeaders.getOrDefault("host", "");
        String host = "";
        int port = 80;

        if(hostHeader.contains(":")) {
            host = hostHeader.split(":")[0];
            port = Integer.parseInt(hostHeader.split(":")[1]);
        } else {
            host = hostHeader;
            port = 80;
        }

        String path = req.path();
        String queryString = req.query();
        Map<String, String> query = new HashMap<String, String>();

        for (String param : queryString.split("&")) {
            String[] pair = param.split("=");

            if(pair.length == 2 && pair[0] != null && !pair[0].isEmpty()) {
                query.put(pair[0], pair[1]);
            }
        }

        String url = scheme + "://" + host;

        if(port != 80) {
            url += ":" + String.valueOf(port);
        }

        url += path;

        if(!queryString.isEmpty()) {
            url += "?" + queryString;
        }

        RuntimeRequest runtimeRequest = new RuntimeRequest(bodyString, body, headers, method, url, scheme, host, path, port, queryString, query);
        RuntimeResponse runtimeResponse = new RuntimeResponse();
        RuntimeContext context = new RuntimeContext(runtimeRequest, runtimeResponse);

        PrintStream systemOut = System.out;
        PrintStream systemErr = System.err;
        
        ByteArrayOutputStream customstdStream = new ByteArrayOutputStream();
        PrintStream customstd = new PrintStream(customstdStream);
        System.setOut(customstd);
        System.setErr(customstd);

        RuntimeOutput output = null;

        try {
            Class classToLoad = Class.forName("io.openruntimes.java.Tests");
            Method classMethod = classToLoad.getDeclaredMethod("main", RuntimeContext.class);
            Object instance = classToLoad.newInstance();
            output = (RuntimeOutput) classMethod.invoke(instance, context);
        } catch (Exception e) {
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            e.printStackTrace(pw);

            context.error(sw.toString());
            output = context.res.send("", 500, new HashMap<String, String>());
        } finally {
            System.out.flush();
            System.err.flush();
            System.setOut(systemOut);
            System.setErr(systemErr);
        }

        if(output == null) {
            context.error("Return statement missing. return context.res.empty() if no response is expected.");
            output = context.res.send("", 500, new HashMap<String, String>());
        }
        
        for (Map.Entry<String, String> entry : output.headers.entrySet()) {
            String header = entry.getKey().toLowerCase();
            if(!(header.startsWith("x-open-runtimes-"))) {
                resp = resp.header(header, entry.getValue());
            }
	    }

        if(!customstdStream.toString().isEmpty()) {
            context.log("Unsupported log noticed. Use context.log() or context.error() for logging.");
        }

        try {
            resp = resp.header("x-open-runtimes-logs", URLEncoder.encode(String.join("\n", context.logs), StandardCharsets.UTF_8.toString()));
            resp = resp.header("x-open-runtimes-errors", URLEncoder.encode(String.join("\n", context.errors), StandardCharsets.UTF_8.toString()));
        } catch (UnsupportedEncodingException ex) {
            resp = resp.header("x-open-runtimes-logs", "Internal error while processing logs.");
            resp = resp.header("x-open-runtimes-errors", "Internal error while processing logs.");
        }

        return resp.code(output.statusCode).result(output.body);
    }
}