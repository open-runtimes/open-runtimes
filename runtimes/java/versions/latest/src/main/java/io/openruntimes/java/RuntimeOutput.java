package io.openruntimes.java;

import java.util.Map;

public class RuntimeOutput {
  private final byte[] body;
  private final Iterable<?> stream;
  private final int statusCode;
  private final Map<String, String> headers;

  public RuntimeOutput(byte[] body, int statusCode, Map<String, String> headers) {
    this.body = body;
    this.stream = null;
    this.statusCode = statusCode;
    this.headers = headers;
  }

  public RuntimeOutput(Iterable<?> stream, int statusCode, Map<String, String> headers) {
    this.body = new byte[0];
    this.stream = stream;
    this.statusCode = statusCode;
    this.headers = headers;
  }

  public byte[] getBody() {
    return this.body;
  }

  public Iterable<?> getStream() {
    return this.stream;
  }

  public int getStatusCode() {
    return this.statusCode;
  }

  public Map<String, String> getHeaders() {
    return this.headers;
  }
}
