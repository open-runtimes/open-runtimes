package io.openruntimes.java;

import java.util.Map;

public class RuntimeOutput {
    private final byte[] body;
    private final int statusCode;
    private final Map<String, String> headers;
    private final boolean chunked;

    public RuntimeOutput(
            byte[] body,
            int statusCode,
            Map<String, String> headers,
            boolean chunked
    ) {
        this.body = body;
        this.statusCode = statusCode;
        this.headers = headers;
        this.chunked = chunked;
    }

    public byte[] getBody() {
        return this.body;
    }

    public int getStatusCode() {
        return this.statusCode;
    }

    public Map<String, String> getHeaders() {
        return this.headers;
    }

    public boolean getChunkedStatus() {
        return this.chunked;
    }

}
