package io.openruntimes.java;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import java.util.Map;

public class RuntimeResponse {
    public Object data;
    public int statusCode;

    static final Gson gson = new GsonBuilder()
        .serializeNulls()
        .create();

    public RuntimeResponse() {
    }

    public RuntimeResponse(String data, int statusCode) {
        this.data = data;
        this.statusCode = statusCode;
    }

    public RuntimeResponse json(Map<String, Object> data) {
        this.data = data;
        this.statusCode = 200;
        return this;
    }

    public RuntimeResponse json(Map<String, Object> data, int statusCode) {
        this.data = data;
        this.statusCode = statusCode;
        return this;
    }

    public RuntimeResponse send(String data) {
        this.data = data;
        this.statusCode = 200;
        return this;
    }

    public RuntimeResponse send(String data, int statusCode) {
        this.data = data;
        this.statusCode = statusCode;
        return this;
    }
}
