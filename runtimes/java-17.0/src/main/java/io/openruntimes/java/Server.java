package io.openruntimes.java;

import org.rapidoid.http.Req;
import org.rapidoid.http.Resp;
import org.rapidoid.setup.On;
import java.io.StringWriter;
import java.io.PrintWriter;

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

        try {
            return resp.result(codeWrapper.main(request, response).data);
        } catch (Exception e) {
            StringWriter sw = new StringWriter();
            PrintWriter pw = new PrintWriter(sw);
            e.printStackTrace(pw);

            return resp.code(500).result(sw.toString());
        }
    }
}