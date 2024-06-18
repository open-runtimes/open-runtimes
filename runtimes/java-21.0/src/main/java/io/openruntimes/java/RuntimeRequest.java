package io.openruntimes.java;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonSyntaxException;

import java.util.Arrays;
import java.util.Map;

public class RuntimeRequest {
    private static final Gson gson = new GsonBuilder().serializeNulls().create();

    private final String method;
    private final String scheme;
    private final String host;
    private final int port;
    private final String path;
    private final Map<String, String> query;
    private final String queryString;
    private final Map<String, String> headers;
    private final byte[] bodyBinary;
    private final String url;

    public RuntimeRequest(
            String method,
            String scheme,
            String host,
            int port,
            String path,
            Map<String, String> query,
            String queryString,
            Map<String, String> headers,
            byte[] bodyBinary,
            String url
    ) {
        this.method = method;
        this.scheme = scheme;
        this.host = host;
        this.port = port;
        this.path = path;
        this.query = query;
        this.queryString = queryString;
        this.headers = headers;
        this.bodyBinary = bodyBinary;
        this.url = url;
    }

    public String getMethod() {
        return method;
    }

    public String getScheme() {
        return scheme;
    }

    public String getHost() {
        return host;
    }

    public int getPort() {
        return port;
    }

    public String getPath() {
        return path;
    }

    public Map<String, String> getQuery() {
        return query;
    }

    public String getQueryString() {
        return queryString;
    }

    public Map<String, String> getHeaders() {
        return headers;
    }

    public byte[] getBodyBinary() {
        return bodyBinary;
    }

    public Object getBody() {
        var contentType = headers.getOrDefault("content-type","text/plain").toLowerCase();

        if (contentType.startsWith("application/json"))
        {
            return getBodyJson();
        }

        String[] binaryTypes = { "application/", "audio/", "font/", "image/", "video/" };

        if(Arrays.stream(binaryTypes).anyMatch(contentType::startsWith)){
            return getBodyBinary();
        }

        return getBodyText();
    }

    public String getBodyText() {
        return new String(bodyBinary);
    }

    public Map<String, Object> getBodyJson() throws JsonSyntaxException{
        if(getBodyText().isEmpty()){
            throw new JsonSyntaxException("Body is empty");
        }
        return gson.fromJson(getBodyText(), Map.class);
    }

    public String getBodyRaw() {
        return getBodyText();
    }

    public String getUrl() {
        return url;
    }
}


