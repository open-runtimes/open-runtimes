package io.openruntimes.java;

import java.io.IOException;

public class RuntimeContext {
  private final RuntimeRequest req;
  private final RuntimeResponse res;
  private final RuntimeLogger logger;

  public RuntimeRequest getReq() {
    return this.req;
  }

  public RuntimeResponse getRes() {
    return this.res;
  }

  public RuntimeContext(RuntimeRequest req, RuntimeResponse res, RuntimeLogger logger) {
    this.req = req;
    this.res = res;
    this.logger = logger;
  }

  public void log(Object... messages) {
    try {
      this.logger.write(messages, RuntimeLogger.TYPE_LOG, false);
      String[] linebreak = new String[1];
      linebreak[0] = "\n";
      this.logger.write(linebreak, RuntimeLogger.TYPE_LOG, false);
    } catch (IOException e) {
      // Ignore missing logs
    }
  }

  public void error(Object... messages) {
    try {
      this.logger.write(messages, RuntimeLogger.TYPE_ERROR, false);
      String[] linebreak = new String[1];
      linebreak[0] = "\n";
      this.logger.write(linebreak, RuntimeLogger.TYPE_ERROR, false);
    } catch (IOException e) {
      // Ignore missing logs
    }
  }
}
