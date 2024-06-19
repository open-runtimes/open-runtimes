package io.openruntimes.java;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import javax.servlet.http.HttpServletResponse;

import java.util.HashMap;
import java.util.Map;

public class RuntimeResponse {
    private static final Gson gson = new GsonBuilder().serializeNulls().create();
    private final HttpServletResponse res;
    private final RuntimeLogger logger;
    private boolean chunkHeaderSent = false;

    public RuntimeResponse(HttpServletResponse res, RuntimeLogger logger) {
        this.res = res;
        this.logger = logger;
    }


    public boolean getChunkStatus() {
        return chunkHeaderSent;
    }

    public RuntimeOutput binary(byte[] body, int statusCode, Map<String, String> headers) {
        return new RuntimeOutput(body, statusCode, headers, false);
    }

    public RuntimeOutput binary(byte[] body, int statusCode) {
        return this.binary(body, statusCode, new HashMap<>());
    }

    public RuntimeOutput binary(byte[] body) {
        return this.binary(body, 200, new HashMap<>());
    }

    public RuntimeOutput send(String body, int statusCode, Map<String, String> headers) {
        return this.text(body, statusCode, headers);
    }

    public RuntimeOutput send(String body, int statusCode) {
        return this.send(body, statusCode, new HashMap<>());
    }

    public RuntimeOutput send(String body) {
        return this.send(body, 200, new HashMap<>());
    }


    public RuntimeOutput text(String body, int statusCode, Map<String, String> headers) {
        return this.binary(body.getBytes(), statusCode, headers);
    }

    public RuntimeOutput text(String body, int statusCode) {
        return this.text(body, statusCode, new HashMap<>());
    }

    public RuntimeOutput text(String body) {
        return this.text(body, 200, new HashMap<>());
    }

    public RuntimeOutput json(Map<String, Object> json, int statusCode, Map<String, String> headers) {
        headers.put("content-type", "application/json");
        return this.text(gson.toJson(json), statusCode, headers);
    }

    public RuntimeOutput json(Map<String, Object> json, int statusCode) {
        return this.json(json, statusCode, new HashMap<>());
    }

    public RuntimeOutput json(Map<String, Object> json) {
        return this.json(json, 200, new HashMap<>());
    }

    public RuntimeOutput empty() {
        return this.text("", 204, new HashMap<>());
    }

    public RuntimeOutput redirect(String url, int statusCode, Map<String, String> headers) {
        headers.put("location", url);
        return this.text("", statusCode, headers);
    }

    public RuntimeOutput redirect(String url, int statusCode) {
        return this.redirect(url, statusCode, new HashMap<>());
    }

    public RuntimeOutput redirect(String url) {
        return this.redirect(url, 301, new HashMap<>());
    }

    public void start() throws Exception {
        this.start(200, new HashMap<>());
    }

    public void start(int statusCode) throws Exception {
        this.start(statusCode, new HashMap<>());
    }

    public void start(int statusCode, Map<String, String> headers) throws Exception {
        if (!this.chunkHeaderSent) {
            this.chunkHeaderSent = true;

            headers.putIfAbsent("cache-control", "no-store");
            headers.putIfAbsent("content-type", "text/event-stream");
            headers.putIfAbsent("connection", "keep-alive");
            headers.putIfAbsent("transfer-encoding", "chunked");
            headers.putIfAbsent("x-open-runtimes-log-id",this.logger.getId());

            for (Map.Entry<String, String> entry : headers.entrySet()) {
                this.res.setHeader(entry.getKey(), entry.getValue());
            }

            this.res.setStatus(statusCode);
            this.res.getOutputStream().flush();
        } else {
            throw new Exception("You can only call res.start() once");
        }
    }

    public void writeJson(Object body) throws Exception {
        this.writeText(gson.toJson(body));
    }

    public void writeText(String body) throws Exception {
        this.writeBinary(body.getBytes());
    }

    public void writeBinary(byte[] body) throws Exception {
        if (!this.chunkHeaderSent) {
            throw new Exception("You must call res.start() to start a chunk response");
        }

        this.res.getOutputStream().write(body);
        this.res.getOutputStream().flush();
    }

    public RuntimeOutput end() throws Exception {
        return this.end(new HashMap<>());
    }

    public RuntimeOutput end(Map<String, String> headers) throws Exception {
        if (!this.chunkHeaderSent) {
            throw new Exception("You must call res.start() to start a chunk response");
        }

        return new RuntimeOutput("".getBytes(), 0, headers, true);
    }
}
