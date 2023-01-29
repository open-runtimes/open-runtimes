package io.openruntimes.java;

import org.rapidoid.http.Req;

import java.util.Map;

public class RuntimeRequest {
    public String rawBody;
    public Object body;
    public Map<String, String> headers;
    public String method;
    public String url;

    public RuntimeRequest(
            String rawBody,
            Object body,
            Map<String, String> headers,
            String method,
            String url
    ) {
        this.rawBody = rawBody;
        this.body = body;
        this.headers = headers;
        this.method = method;
        this.url = url;
    }
}


