package io.openruntimes.java;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import java.util.HashMap;
import java.util.Map;

public class RuntimeResponse {
  private static final Gson gson = new GsonBuilder().serializeNulls().create();

  public RuntimeOutput binary(byte[] bytes, int statusCode, Map<String, String> headers) {
    return new RuntimeOutput(bytes, statusCode, headers);
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
}
