package io.openruntimes.java;

import java.util.Map;
import java.util.HashMap;
import java.net.HttpURLConnection;
import java.net.URL;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import com.google.gson.Gson;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;

public class Tests {
    final Gson gson = new Gson();
    public RuntimeOutput main(RuntimeContext context) throws Exception {
        String action = context.getReq().getHeaders().getOrDefault("x-action", "");

        Map<String, Object> json = new HashMap<>();
        Map<String, String> headers = new HashMap<String, String>();

        switch (action) {
            case "plaintextResponse":
                return context.getRes().send("Hello World 👋");
            case "jsonResponse":
                json.put("json", true);
                json.put("message", "Developers are awesome.");
                return context.getRes().json(json);
            case "customCharsetResponse":
                headers.put("content-type", "text/plain; charset=iso-8859-1");
                return context.getRes().send("ÅÆ", 200, headers);
            case "multipartResponse":
                headers.put("content-type", "multipart/form-data; boundary=12345");
                return context.getRes().send("--12345\n" +
                        "Content-Disposition: form-data; name=\"partOne\"\n" +
                        "\n" +
                        "Why just have one part?\n" +
                        "--12345\n" +
                        "Content-Disposition: form-data; name=\"partTwo\"\n" +
                        "\n" +
                        "When you can have two!\n" +
                        "--12345--", 200, headers);
            case "redirectResponse":
                return context.getRes().redirect("https://github.com/");
            case "emptyResponse":
                return context.getRes().empty();
            case "noResponse":
                context.getRes().send("This should be ignored, as it is not returned.");

                // Simulate test data. Return nessessary in Java
                context.error("Return statement missing. return context.getRes().empty() if no response is expected.");
                return context.getRes().send("", 500);
            case "doubleResponse":
                context.getRes().send("This should be ignored.");
                return context.getRes().send("This should be returned.");
            case "headersResponse":
                headers.put("first-header", "first-value");
                headers.put("second-header", context.getReq().getHeaders().getOrDefault("x-open-runtimes-custom-in-header", "missing"));
                headers.put("cookie", context.getReq().getHeaders().getOrDefault("cookie", "missing"));
                headers.put("x-open-runtimes-custom-out-header", "third-value");
                return context.getRes().send("OK", 200, headers);
            case "statusResponse":
                return context.getRes().send("FAIL", 404);
            case "requestMethod":
                return context.getRes().send(context.getReq().getMethod());
            case "requestUrl":
                json.put("url", context.getReq().getUrl());
                json.put("port", context.getReq().getPort());
                json.put("path", context.getReq().getPath());
                json.put("query", context.getReq().getQuery());
                json.put("queryString", context.getReq().getQueryString());
                json.put("scheme", context.getReq().getScheme());
                json.put("host", context.getReq().getHost());
                return context.getRes().json(json);
            case "requestHeaders":
                for (Map.Entry<String, String> entry : context.getReq().getHeaders().entrySet()) {
                    json.put(entry.getKey(), entry.getValue());
                }
                return context.getRes().json(json);
            case "requestBodyPlaintext":
                return context.getRes().send((String) context.getReq().getBody());
            case "requestBodyJson":
                String key1 = "";
                String key2 = "";
                if (context.getReq().getBody() instanceof String) {
                    key1 = "Missing key";
                    key2 = "Missing key";
                } else {
                    Map<String, Object> body = (Map<String, Object>) context.getReq().getBody();

                    key1 = body.getOrDefault("key1", "Missing key").toString();
                    key2 = body.getOrDefault("key2", "Missing key").toString();
                }
                json.put("key1", key1);
                json.put("key2", key2);
                json.put("raw", context.getReq().getBodyRaw());
                return context.getRes().json(json);
            case "envVars":
                json.put("var", System.getenv().getOrDefault("CUSTOM_ENV_VAR", null));
                json.put("emptyVar", System.getenv().getOrDefault("NOT_DEFINED_VAR", null));
                return context.getRes().json(json);
            case "logs":
                System.out.println("Native log");
                context.log("Debug log");
                context.error("Error log");
                context.log(42);
                context.log(4.2);
                context.log(true);
                ArrayList<String> array = new ArrayList<String>();
                array.add("arrayValue");
                context.log(array);
                HashMap<String, String> map = new HashMap<String, String>();
                map.put("objectKey", "objectValue");
                context.log(map);
                return context.getRes().send("");
            case "library":
                URL url = new URL("https://jsonplaceholder.typicode.com/todos/" + context.getReq().getBodyRaw());
                HttpURLConnection con = (HttpURLConnection) url.openConnection();
                con.setRequestMethod("GET");
                con.getResponseCode();
                BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
                String inputLine;
                StringBuilder todoBuffer = new StringBuilder();
                while ((inputLine = in.readLine()) != null) {
                    todoBuffer.append(inputLine);
                }
                in.close();
                con.disconnect();
                Map<String, Object> todo = gson.fromJson(todoBuffer.toString(), Map.class);
                json.put("todo", todo);
                return context.getRes().json(json);
            case "timeout":
                context.log("Timeout start.");

                Executors.newCachedThreadPool().submit(() -> {
                    try {
                        Thread.sleep(3000);
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                }).get();

                context.log("Timeout end.");

                return context.getRes().send("Successful response.");
            default:
                throw new Exception("Unknown action");
        }
    }
}
