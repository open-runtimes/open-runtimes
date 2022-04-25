package io.openruntimes.java;

import org.rapidoid.http.Req;

import java.util.Map;

public class RuntimeRequest {
    private String payload;
    private Map<String, String> headers;
    private Map<String, String> env;

    public RuntimeRequest(
            String payload,
            Map<String, String> headers,
            Map<String, String> env
    ) {
        this.payload = payload;
        this.headers = headers;
        this.env = env;
    }

    public RuntimeRequest(Req request) {
        Map<String, Object> data = request.data();
        if (data.containsKey("payload")) {
            this.payload = (String) data.get("payload");
        } else {
            this.payload = "";
        }
        if (data.containsKey("headers")) {
            this.headers = (Map<String, String>) data.get("headers");
        }
        if (data.containsKey("env")) {
            this.env = (Map<String, String>) data.get("env");
        }
    }

    public RuntimeRequest() {
    }

    public String getPayload() {
        return payload;
    }

    public void setPayload(String payload) {
        this.payload = payload;
    }

    public Map<String, String> getHeaders() {
        return headers;
    }

    public void setHeaders(Map<String, String> headers) {
        this.headers = headers;
    }

    public Map<String, String> getEnv() {
        return env;
    }

    public void setEnv(Map<String, String> env) {
        this.env = env;
    }
}


