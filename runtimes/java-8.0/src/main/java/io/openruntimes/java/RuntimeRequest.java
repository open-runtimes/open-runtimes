package io.openruntimes.java;

import org.rapidoid.http.Req;

import java.util.Map;

public class RuntimeRequest {
    private String payload;
    private Map<String, String> headers;
    private Map<String, String> variables;

    public RuntimeRequest(
            String payload,
            Map<String, String> headers,
            Map<String, String> variables
    ) {
        this.payload = payload;
        this.headers = headers;
        this.variables = variables;
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
        if (data.containsKey("variables")) {
            this.variables = (Map<String, String>) data.get("variables");
        }
    }

    public RuntimeRequest() {
    }

    public String getPayload() {
        return this.payload;
    }

    public void setPayload(String payload) {
        this.payload = payload;
    }

    public Map<String, String> getHeaders() {
        return this.headers;
    }

    public void setHeaders(Map<String, String> headers) {
        this.headers = headers;
    }

    public Map<String, String> getVariables() {
        return this.variables;
    }

    public void setVariables(Map<String, String> variables) {
        this.variables = variables;
    }
}


