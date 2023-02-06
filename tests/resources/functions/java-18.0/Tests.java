package io.openruntimes.java;

import java.util.Map;
import java.util.HashMap;
import java.net.HttpURLConnection;
import java.net.URL;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import com.google.gson.Gson;

public class Tests {
    final Gson gson = new Gson();

    public RuntimeOutput main(RuntimeContext context) throws Exception {
        String action = context.req.headers.getOrDefault("x-action", "");

        Map<String, Object> json = new HashMap<String, Object>();

        switch (action) {
            case "plaintextResponse":
                return context.res.send("Hello World 👋");
            case "jsonResponse":
                json.put("json", true);
                json.put("message", "Developers are awesome.");
                return context.res.json(json);
            case "redirectResponse":
                return context.res.redirect("https://github.com/");
            case "emptyResponse":
                return context.res.empty();
            case "noResponse":
                context.res.send("This should be ignored, as it is not returned.");

                // Simulate test data. Return nessessary in Java
                context.error("Return statement missing. return context.res.empty() if no response is expected.");
                return context.res.send("", 500);
            case "doubleResponse":
                context.res.send("This should be ignored.");
                return context.res.send("This should be returned.");
            case "headersResponse":
                Map<String, String> headers = new HashMap<String, String>();
                headers.put("first-header", "first-value");
                headers.put("second-header", context.req.headers.getOrDefault("x-open-runtimes-custom-in-header", "missing"));
                headers.put("x-open-runtimes-custom-out-header", "third-value");
                return context.res.send("OK", 200, headers);
            case "statusResponse":
                return context.res.send("FAIL", 404);
            case "requestMethod":
                return context.res.send(context.req.method);
            case "requestUrl":
                return context.res.send(context.req.url);
            case "requestHeaders":
                for (Map.Entry<String, String> entry : context.req.headers.entrySet()) {
                    json.put(entry.getKey(), entry.getValue());
                }

                return context.res.json(json);
            case "requestBodyPlaintext":
                return context.res.send((String) context.req.body);
            case "requestBodyJson":
                String key1 = "";
                String key2 = "";

                if(context.req.body instanceof String) {
                    key1 = "Missing key";
                    key2 = "Missing key";
                } else {
                    Map<String, Object> body = (Map<String, Object>) context.req.body;

                    key1 = body.getOrDefault("key1", "Missing key").toString();
                    key2 = body.getOrDefault("key2", "Missing key").toString();
                }

                json.put("key1", key1);
                json.put("key2", key2);
                json.put("raw", context.req.rawBody);
                return context.res.json(json);
            case "envVars":
                json.put("var", System.getenv().getOrDefault("CUSTOM_ENV_VAR", null));
                json.put("emptyVar", System.getenv().getOrDefault("NOT_DEFINED_VAR", null));
                return context.res.json(json);
            case "logs":
                System.out.println("Native log");
                context.log("Debug log");
                context.error("Error log");
                
                context.log(42);
                context.log(4.2);
                context.log(true);

                return context.res.send("");
            default:
                throw new Exception("Unkonwn action");
        }
    }
}
