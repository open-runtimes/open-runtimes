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

public class Server {

    public static void main(String[] args) {
        On.port(3000);
        On.post("/").plain(Server::execute);
    }

    public static Resp execute(Req req, Resp resp) {
        if (!req.headers().containsKey("x-internal-challenge") || req.headers().get("x-internal-challenge").isEmpty()) {
            return resp.code(500).result("Unauthorized");
        }
        if (!req.headers().get("x-internal-challenge").equals(System.getenv("INTERNAL_RUNTIME_KEY"))) {
            return resp.code(500).result("Unauthorized");
        }

        Wrapper codeWrapper = new Wrapper();
        RuntimeRequest request = new RuntimeRequest(req);
        RuntimeResponse response = new RuntimeResponse();

        ByteArrayOutputStream outStream = new ByteArrayOutputStream();
        ByteArrayOutputStream errStream = new ByteArrayOutputStream();
        PrintStream userOut = new PrintStream(outStream);
        PrintStream userErr = new PrintStream(errStream);
        PrintStream systemOut = System.out;
        PrintStream systemErr = System.err;

        System.setOut(userOut);
        System.setErr(userErr);

        try {
            RuntimeResponse userResponse = codeWrapper.main(request, response);
            Map<String, Object> output = new HashMap<String, Object>();
            output.put("response", userResponse.data);
            output.put("stdout", outStream.toString());
            return resp.result(RuntimeResponse.gson.toJson(output));
        } catch (Exception e) {
            e.printStackTrace();
            Map<String, Object> output = new HashMap<String, Object>();
            output.put("stdout", outStream.toString());
            output.put("stderr", errStream.toString());
            return resp.code(500).result(RuntimeResponse.gson.toJson(output));
        } finally {
            System.out.flush();
            System.err.flush();
            System.setOut(systemOut);
            System.setErr(systemErr);
        }
    }
}