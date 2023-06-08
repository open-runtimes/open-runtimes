package io.openruntimes.java;

import org.rapidoid.http.Req;

import java.util.Map;

public class RuntimeRequest {
    private String bodyString;
    private Object body;
    private Map<String, String> headers;
    private String method;
    private String url;
    private String scheme;
    private String host;
    private String path;
    private int port;
    private String queryString;
    private Map<String, String> query;

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

    public String getBodyString() {
        return this.bodyString;
    }

    public Object getBody() {
        return this.body;
    }

    public Map<String, String> getHeaders() {
        return this.headers;
    }

    public String getMethod() {
        return this.method;
    }

    public String getUrl() {
        return this.url;
    }

    public String getScheme() {
        return this.scheme;
    }

    public String getHost() {
        return this.host;
    }

    public String getPath() {
        return this.path;
    }

    public int getPort() {
        return this.port;
    }

    public String getQueryString() {
        return this.queryString;
    }

    public Map<String, String> getQuery() {
        return this.query;
    }
}


