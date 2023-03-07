package io.openruntimes.java;

import java.util.Map;

public class RuntimeOutput {
    private String body;
    private int statusCode;
    private Map<String, String> headers;

    public RuntimeOutput(
            String body,
            int statusCode,
            Map<String, String> headers
    ) {
        this.body = body;
        this.statusCode = statusCode;
        this.headers = headers;
    }

    public String getBody() {
        return this.body;
    }

    public int getStatusCode() {
        return this.statusCode;
    }

    public Map<String, String> getHeaders() {
        return this.headers;
    }
}
