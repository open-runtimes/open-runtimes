package io.openruntimes.java;

import org.rapidoid.http.Req;

import java.util.Map;

public class RuntimeRequest {
    public String bodyString;
    public Object body;
    public Map<String, String> headers;
    public String method;
    public String url;
    public String scheme;
    public String host;
    public String path;
    public int port;
    public String queryString;
    public Map<String, String> query;

    public RuntimeRequest(
            String url,
            String method,
            String scheme,
            String host,
            int port,
            String path,
            Map<String, String> query,
            String queryString,
            Map<String, String> headers,
            Object body,
            String bodyString
    ) {
        this.bodyString = bodyString;
        this.body = body;
        this.headers = headers;
        this.method = method;
        this.url = url;
        this.scheme = scheme;
        this.host = host;
        this.path = path;
        this.port = port;
        this.queryString = queryString;
        this.query = query;
    }
}


