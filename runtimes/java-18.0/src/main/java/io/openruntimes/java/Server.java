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

        if(!reqHeaders.getOrDefault("x-open-runtimes-secret", "").equals(System.getenv("OPEN_RUNTIMES_SECRET"))) {
            return resp.code(500).result("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.");
        }

        String rawBody = req.body() == null ? "" : new String(req.body(), StandardCharsets.UTF_8);
        Object body = rawBody;
        Map<String, String> headers = new HashMap<String, String>();
        String method = req.verb();
        String url = req.uri();

        for (Map.Entry<String, String> entry : reqHeaders.entrySet()) {
            String header = entry.getKey().toLowerCase();
            if(!(header.startsWith("x-open-runtimes-"))) {
                headers.put(header, entry.getValue());
            }
	    }

        String contentType = reqHeaders.getOrDefault("content-type", "text/plain");
        if(contentType.contains("application/json")) {
            Gson gson = new GsonBuilder().serializeNulls().create();
            body = gson.fromJson(rawBody, Map.class);
        }

        RuntimeRequest runtimeRequest = new RuntimeRequest(rawBody, body, headers, method, url);
        RuntimeResponse runtimeResponse = new RuntimeResponse();
        RuntimeContext context = new RuntimeContext(runtimeRequest, runtimeResponse);

        ByteArrayOutputStream customstdStream = new ByteArrayOutputStream();
        PrintStream customstd = new PrintStream(customstdStream);
        System.setOut(customstd);
        System.setErr(customstd);

        RuntimeOutput output = null;

        try {
            Wrapper codeWrapper = new Wrapper();
            output = codeWrapper.main(context);
        } catch (Exception e) {
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            e.printStackTrace(pw);

            context.error(sw.toString());
            output = context.res.send("", 500, new HashMap<String, String>());
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