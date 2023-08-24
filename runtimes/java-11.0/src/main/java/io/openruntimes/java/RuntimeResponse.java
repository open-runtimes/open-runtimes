package io.openruntimes.java;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import java.util.HashMap;
import java.util.Map;

public class RuntimeResponse {
    private static final Gson gson = new GsonBuilder().serializeNulls().create();

    public RuntimeOutput send(String body, int statusCode, Map<String, String> headers) {
        return new RuntimeOutput(body, statusCode, headers);
    }

    public RuntimeOutput send(String body, int statusCode) {
        return this.send(body, statusCode, new HashMap<>());
    }

    public RuntimeOutput send(String body) {
        return this.send(body, 200, new HashMap<>());
    }

    public RuntimeOutput json(Map<String, Object> json, int statusCode, Map<String, String> headers) {
        if (!headers.containsKey("content-type")) {
            headers.put("content-type", "application/json");
        }
        return this.send(gson.toJson(json), statusCode, headers);
    }

    public RuntimeOutput json(Map<String, Object> json, int statusCode) {
        return this.json(json, statusCode, new HashMap<>());
    }

    public RuntimeOutput json(Map<String, Object> json) {
        return this.json(json, 200, new HashMap<>());
    }

    public RuntimeOutput empty() {
        return this.send("", 204, new HashMap<>());
    }

    public RuntimeOutput redirect(String url, int statusCode, Map<String, String> headers) {
        headers.put("location", url);
        return this.send("", statusCode, headers);
    }

    public RuntimeOutput redirect(String url, int statusCode) {
        return this.redirect(url, statusCode, new HashMap<>());
    }

    public RuntimeOutput redirect(String url) {
        return this.redirect(url, 301, new HashMap<>());
    }
}
