package io.openruntimes.java;

import org.rapidoid.http.Req;

import java.util.Map;

public class RuntimeRequest {
    private String path;
    private String file;
    private Map<String, String> env;
    private String payload;
    private Map<String, String> headers;

    public RuntimeRequest(
            String path,
            String file,
            Map<String, String> env,
            String payload,
            Map<String, String> headers
    ) {
        this.path = path;
        this.file = file;
        this.env = env;
        this.payload = payload;
        this.headers = headers;
    }

    public RuntimeRequest(Req request) {
        Map<String, Object> data = request.data();
        if (data.containsKey("path")) {
            this.path = (String) data.get("path");
        }
        if (data.containsKey("file")) {
            this.file = (String) data.get("file");
        }
        if (data.containsKey("payload")) {
            this.payload = (String) data.get("payload");
        }
        if (data.containsKey("env")) {
            this.env = (Map<String, String>) data.get("env");
        }
        if (data.containsKey("headers")) {
            this.headers = (Map<String, String>) data.get("headers");
        }
    }

    public RuntimeRequest() {
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getFile() {
        return file;
    }

    public void setFile(String file) {
        this.file = file;
    }

    public Map<String, String> getEnv() {
        return env;
    }

    public void setEnv(Map<String, String> env) {
        this.env = env;
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
}


