package io.openruntimes.java;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.ToNumberPolicy;
import java.util.HashMap;
import java.util.Map;

public final class OprConfig {
  public static final String SECRET;
  public static final String ENTRYPOINT = System.getenv("OPEN_RUNTIMES_ENTRYPOINT");
  public static final String ENV;
  public static final Map<String, String> HEADERS;

  static {
    String secret = System.getenv("OPEN_RUNTIMES_SECRET");
    SECRET = secret == null ? "" : secret;

    String env = System.getenv("OPEN_RUNTIMES_ENV");
    ENV = env == null ? "" : env;

    Map<String, String> headers = new HashMap<>();
    String headersString = System.getenv("OPEN_RUNTIMES_HEADERS");
    if (headersString != null && !headersString.isEmpty()) {
      Gson gson =
          new GsonBuilder()
              .serializeNulls()
              .setObjectToNumberStrategy(ToNumberPolicy.LONG_OR_DOUBLE)
              .create();
      try {
        Map<String, Object> parsed = gson.fromJson(headersString, Map.class);
        for (Map.Entry<String, Object> entry : parsed.entrySet()) {
          headers.put(entry.getKey().toLowerCase(), String.valueOf(entry.getValue()));
        }
      } catch (Exception e) {
        // Ignore invalid enforced headers
      }
    }
    HEADERS = headers;
  }

  private OprConfig() {}
}
