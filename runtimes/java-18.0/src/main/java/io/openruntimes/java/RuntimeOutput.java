package io.openruntimes.java;

import java.util.Map;

public class RuntimeOutput {
    public String body;
    public int statusCode;
    public Map<String, String> headers;

    public RuntimeOutput(
            String body,
            int statusCode,
            Map<String, String> headers
    ) {
        this.body = body;
        this.statusCode = statusCode;
        this.headers = headers;
    }
}
