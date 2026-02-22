package io.openruntimes.java;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.ToNumberPolicy;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.HashMap;
import java.util.concurrent.Executors;

public class Tests {
  public RuntimeOutput main(RuntimeContext context) throws Exception {
    String action = context.getReq().getHeaders().getOrDefault("x-action", "");

    Map<String, Object> json = new HashMap<>();
    Map<String, String> headers = new HashMap<String, String>();
    byte[] bytes = {};

    switch (action) {
      case "plaintextResponse":
        return context.getRes().text("Hello World 👋");
      case "jsonResponse":
        json.put("json", true);
        json.put("message", "Developers are awesome.");
        return context.getRes().json(json);
      case "customCharsetResponse":
        headers.put("content-type", "text/plain; charset=iso-8859-1");
        return context.getRes().text("ÅÆ", 200, headers);
      case "uppercaseCharsetResponse":
        headers.put("content-type", "TEXT/PLAIN");
        return context.getRes().text("ÅÆ", 200, headers);
      case "multipartResponse":
        headers.put("content-type", "multipart/form-data; boundary=12345");
        return context
            .getRes()
            .text(
                "--12345\n"
                    + "Content-Disposition: form-data; name=\"partOne\"\n"
                    + "\n"
                    + "Why just have one part?\n"
                    + "--12345\n"
                    + "Content-Disposition: form-data; name=\"partTwo\"\n"
                    + "\n"
                    + "When you can have two!\n"
                    + "--12345--",
                200,
                headers);
      case "redirectResponse":
        return context.getRes().redirect("https://github.com/");
      case "emptyResponse":
        return context.getRes().empty();
      case "noResponse":
        context.getRes().text("This should be ignored, as it is not returned.");

        // Simulate test data. Return nessessary in Java
        context.error(
            "Return statement missing. return context.getRes().empty() if no response is"
                + " expected.");
        return context.getRes().text("", 500);
      case "doubleResponse":
        context.getRes().text("This should be ignored.");
        return context.getRes().text("This should be returned.");
      case "enforcedHeaders":
        json.put("x-custom", context.getReq().getHeaders().getOrDefault("x-custom", ""));
        json.put(
            "x-custom-uppercase",
            context.getReq().getHeaders().getOrDefault("x-custom-uppercase", ""));
        json.put(
            "x-open-runtimes-custom",
            context.getReq().getHeaders().getOrDefault("x-open-runtimes-custom", ""));

        return context.getRes().json(json);
      case "headersResponse":
        headers.put("first-header", "first-value");
        headers.put(
            "second-header",
            context
                .getReq()
                .getHeaders()
                .getOrDefault("x-open-runtimes-custom-in-header", "missing"));
        headers.put("cookie", context.getReq().getHeaders().getOrDefault("cookie", "missing"));
        headers.put("x-open-runtimes-custom-out-header", "third-value");
        return context.getRes().text("OK", 200, headers);
      case "statusResponse":
        return context.getRes().text("FAIL", 404);
      case "requestMethod":
        return context.getRes().text(context.getReq().getMethod());
      case "requestUrl":
        json.put("url", context.getReq().getUrl());
        json.put("port", context.getReq().getPort());
        json.put("path", context.getReq().getPath());
        json.put("query", context.getReq().getQuery());
        json.put("queryString", context.getReq().getQueryString());
        json.put("scheme", context.getReq().getScheme());
        json.put("host", context.getReq().getHost());
        return context.getRes().json(json);
      case "requestHeaders":
        for (Map.Entry<String, String> entry : context.getReq().getHeaders().entrySet()) {
          json.put(entry.getKey(), entry.getValue());
        }
        return context.getRes().json(json);
      case "requestBodyText":
        return context.getRes().text(context.getReq().getBodyText());
      case "requestBodyJson":
        return context.getRes().json(context.getReq().getBodyJson());
      case "requestBodyBinary":
        return context.getRes().binary(context.getReq().getBodyBinary());
      case "requestBodyTextAuto":
        return context.getRes().text((String) context.getReq().getBody());
      case "requestBodyJsonAuto":
        return context.getRes().json((Map<String, Object>) context.getReq().getBody());
      case "binaryResponse1":
        bytes = new byte[] {0, 10, (byte) 255};
        return context.getRes().binary(bytes); // byte[]
      case "binaryResponse2":
        bytes = new byte[] {0, 20, (byte) 255};
        return context.getRes().binary(bytes); // Just a filler
      case "binaryResponse3":
        bytes = new byte[] {0, 30, (byte) 255};
        return context.getRes().binary(bytes); // Just a filler
      case "binaryResponse4":
        bytes = new byte[] {0, 40, (byte) 255};
        return context.getRes().binary(bytes); // Just a filler
      case "binaryResponse5":
        bytes = new byte[] {0, 50, (byte) 255};
        return context.getRes().binary(bytes); // Just a filler
      case "binaryResponseLarge":
        bytes = context.getReq().getBodyBinary();
        MessageDigest md5Digest = null;
        try {
          md5Digest = MessageDigest.getInstance("MD5");
        } catch (NoSuchAlgorithmException e) {
        }
        md5Digest.update(bytes);
        byte[] digestBytes = md5Digest.digest();
        String hex = bytesToHex(digestBytes).toLowerCase(Locale.ROOT);
        headers.put("x-method", context.getReq().getMethod());
        return context.getRes().text(hex, 200, headers);
      case "envVars":
        json.put("var", System.getenv().getOrDefault("CUSTOM_ENV_VAR", null));
        json.put("emptyVar", System.getenv().getOrDefault("NOT_DEFINED_VAR", null));
        return context.getRes().json(json);
      case "logs":
        System.out.println("Native log");
        context.log("Debug log");
        context.error("Error log");
        context.log("Log+With+Plus+Symbol");
        context.log(42);
        context.log(4.2);
        context.log(true);
        ArrayList<String> array = new ArrayList<String>();
        array.add("arrayValue");
        context.log(array);
        HashMap<String, String> map = new HashMap<String, String>();
        map.put("objectKey", "objectValue");
        context.log(map);
        context.log(new String(new char[9000]).replace('\0', 'A'));
        context.error(new String(new char[9000]).replace('\0', 'B'));
        return context.getRes().text("");
      case "library":
        URL url = new URL("https://dummyjson.com/todos/" + context.getReq().getBodyRaw());
        HttpURLConnection con = (HttpURLConnection) url.openConnection();
        con.setRequestMethod("GET");
        con.setRequestProperty(
            "User-Agent",
            "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko)"
                + " Chrome/91.0.4472.124 Safari/537.36");
        con.getResponseCode();
        BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()));
        String inputLine;
        StringBuilder todoBuffer = new StringBuilder();
        while ((inputLine = in.readLine()) != null) {
          todoBuffer.append(inputLine);
        }
        in.close();
        con.disconnect();
        Gson gson =
            new GsonBuilder()
                .setObjectToNumberStrategy(ToNumberPolicy.LONG_OR_DOUBLE)
                .setNumberToNumberStrategy(ToNumberPolicy.LONG_OR_DOUBLE)
                .create();
        Map<String, Object> todo = gson.fromJson(todoBuffer.toString(), Map.class);
        json.put("todo", todo);
        return context.getRes().json(json);
      case "timeout":
        context.log("Timeout start.");

        Executors.newCachedThreadPool()
            .submit(
                () -> {
                  try {
                    Thread.sleep(3000);
                  } catch (InterruptedException e) {
                    e.printStackTrace();
                  }
                })
            .get();

        context.log("Timeout end.");

        return context.getRes().text("Successful response.");
      case "deprecatedMethods":
        return context.getRes().send(context.getReq().getBodyRaw());
      case "deprecatedMethodsUntypedBody":
        return context.getRes().send("50"); // Send only supported String
      case "spreadOperatorLogs":
        String engine = "open-runtimes";
        context.log("engine:", engine);
        context.error("engine:", engine);
        return context.getRes().text("OK");
      case "errorTest":
        context.log("Before error...");
        throw new Exception("Error!");
      default:
        throw new Exception("Unknown action");
    }
  }

  public static String bytesToHex(byte[] bytes) {

    char[] result = new char[bytes.length * 2];

    for (int index = 0; index < bytes.length; index++) {
      int v = bytes[index];

      int upper = (v >>> 4) & 0xF;
      result[index * 2] = (char) (upper + (upper < 10 ? 48 : 65 - 10));

      int lower = v & 0xF;
      result[index * 2 + 1] = (char) (lower + (lower < 10 ? 48 : 65 - 10));
    }

    return new String(result);
  }
}
