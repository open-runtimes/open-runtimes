package io.openruntimes.java;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import java.io.ByteArrayOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.time.Instant;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.Set;

public class RuntimeLogger {
  public static final String TYPE_ERROR = "error";
  public static final String TYPE_LOG = "log";

  private String id = "";
  private boolean enabled = false;
  private boolean includesNativeInfo = false;

  private FileWriter streamLogs = null;
  private FileWriter streamErrors = null;

  private ByteArrayOutputStream customStdStream;

  private PrintStream nativeLogsCache = null;
  private PrintStream nativeErrorsCache = null;

  private static final Gson gson = new GsonBuilder().serializeNulls().create();

  public RuntimeLogger(String status, String id) throws IOException {
    this.customStdStream = new ByteArrayOutputStream();

    if (status == null) {
      status = "";
    }

    if (id == null) {
      id = "";
    }

    if (status.equals("enabled") || status.equals("")) {
      this.enabled = true;
    } else {
      this.enabled = false;
    }

    if (this.enabled) {
      String serverEnv = System.getenv("OPEN_RUNTIMES_ENV");
      if (serverEnv == null) {
        serverEnv = "";
      }

      if (id.equals("")) {
        if (serverEnv.equals("development")) {
          this.id = "dev";
        } else {
          this.id = this.generateId(7);
        }
      } else {
        this.id = id;
      }

      this.streamLogs = new FileWriter("/mnt/logs/" + this.id + "_logs.log", true);
      this.streamErrors = new FileWriter("/mnt/logs/" + this.id + "_errors.log", true);
    }
  }

  public String getId() {
    return this.id;
  }

  public void write(Object[] messages, String type, Boolean xnative) throws IOException {
    if (this.enabled == false) {
      return;
    }

    if (type == null) {
      type = RuntimeLogger.TYPE_LOG;
    }

    if (xnative == null) {
      xnative = false;
    }

    if (xnative && !this.includesNativeInfo) {
      this.includesNativeInfo = true;

      String[] logs = new String[1];
      logs[0] = "Native logs detected. Use context.log() or context.error() for better experience.";
      this.write(logs, type, xnative);
    }

    FileWriter stream = this.streamLogs;

    if (type == RuntimeLogger.TYPE_ERROR) {
      stream = this.streamErrors;
    }

    String stringLog = "";

    int i = 0;

    for (Object message : messages) {
      if (message instanceof Map || message instanceof List || message instanceof Set) {
        stringLog += gson.toJson(message);
      } else {
        stringLog += message.toString();
      }

      if (i < messages.length - 1) {
        stringLog += " ";
      }

      i += 1;
    }

    if (stringLog.length() > 8000) {
      stringLog = stringLog.substring(0, 8000);
      stringLog += "... Log truncated due to size limit (8000 characters)";
    }

    try {
      stream.write(stringLog);
    } catch (IOException e) {
      // Silently fail to prevent 500 errors in runtime
      // Log write failures should not crash the runtime
    }
  }

  public void end() throws IOException {
    if (!this.enabled) {
      return;
    }

    this.enabled = false;

    this.streamLogs.close();
    this.streamErrors.close();
  }

  public void overrideNativeLogs() {
    this.nativeLogsCache = System.out;
    this.nativeErrorsCache = System.err;

    PrintStream customStd = new PrintStream(this.customStdStream);
    System.setOut(customStd);
    System.setErr(customStd);
  }

  public void revertNativeLogs() {
    System.out.flush();
    System.err.flush();

    System.setOut(this.nativeLogsCache);
    System.setErr(this.nativeErrorsCache);

    if (!this.customStdStream.toString().isEmpty()) {
      try {
        String[] logs = new String[1];
        logs[0] = customStdStream.toString();
        this.write(logs, RuntimeLogger.TYPE_LOG, true);
      } catch (IOException e) {
        // Ignore missing logs
      }
    }
  }

  private String generateId(int padding) {
    Instant now = Instant.now();
    long sec = now.getEpochSecond();
    long usec = (System.nanoTime() / 1000l) % 1000l;

    String baseId = String.format("%08x%05x", sec, usec);

    Random random = new Random();
    String randomPadding = "";
    for (int i = 0; i < padding; i++) {
      randomPadding = randomPadding + Integer.toHexString(random.nextInt(16));
    }

    return baseId + randomPadding;
  }
}
