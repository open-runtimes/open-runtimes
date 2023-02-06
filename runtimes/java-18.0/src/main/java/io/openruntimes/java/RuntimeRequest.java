package io.openruntimes.java;

import org.rapidoid.http.Req;

import java.util.Map;

public class RuntimeRequest {
    public String bodyString;
    public Object body;
    public Map<String, String> headers;
    public String method;
    public String url;

    public RuntimeRequest(
            String bodyString,
            Object body,
            Map<String, String> headers,
            String method,
            String url
    ) {
        this.bodyString = bodyString;
        this.body = body;
        this.headers = headers;
        this.method = method;
        this.url = url;
    }
}


