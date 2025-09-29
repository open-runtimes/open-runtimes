#ifndef CPP_RUNTIME_RUNTIMELOGGER_H
#define CPP_RUNTIME_RUNTIMELOGGER_H

#include <chrono>
#include <cstdlib>
#include <ctime>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <sstream>
#include <string>

namespace runtime {
class RuntimeLogger {
public:
  bool enabled = false;
  std::string id = "";
  bool includesNativeInfo = false;

  std::shared_ptr<std::ofstream> streamLogs;
  std::shared_ptr<std::ofstream> streamErrors;

  std::shared_ptr<std::stringstream> customStdStreamLogs;
  std::shared_ptr<std::stringstream> customStdStreamErrors;

  std::streambuf *nativeLogsCache;
  std::streambuf *nativeErrorsCache;

  RuntimeLogger(std::string headerStatus, std::string headerId) {
    if (headerStatus == "" || headerStatus == "enabled") {
      enabled = true;
    }

    if (enabled == true) {
      std::string serverEnv = "";
      if (std::getenv("OPEN_RUNTIMES_ENV") != nullptr) {
        serverEnv = std::getenv("OPEN_RUNTIMES_ENV");
      }

      if (headerId == "") {
        if (serverEnv == "development") {
          id = "dev";
        } else {
          id = generateId();
        }
      } else {
        id = headerId;
      }

      streamLogs = std::make_shared<std::ofstream>(
          "/mnt/logs/" + id + "_logs.log", std::ios_base::app);
      streamErrors = std::make_shared<std::ofstream>(
          "/mnt/logs/" + id + "_errors.log", std::ios_base::app);
    }
  }

  void write(const std::string message, std::string type = "",
             const bool native = false) {
    if (enabled == false) {
      return;
    }

    if (type == "") {
      type = "log";
    }

    if (native && !includesNativeInfo) {
      includesNativeInfo = true;
      write("Native logs detected. Use context.log() or context.error() for "
            "better experience.",
            type, native);
    }

    std::shared_ptr<std::ofstream> stream =
        type == "error" ? streamErrors : streamLogs;

    std::string truncatedMessage = message;
    if (truncatedMessage.length() > 8000) {
      truncatedMessage = truncatedMessage.substr(0, 8000);
      truncatedMessage += "... Log truncated due to size limit (8000 characters)";
    }

    try {
      *(stream) << (truncatedMessage + "\n");
    } catch (const std::exception &e) {
      // Silently fail to prevent 500 errors in runtime
      // Log write failures should not crash the runtime
    }
  }

  void end() {
    if (enabled == false) {
      return;
    }

    enabled = false;

    streamLogs->close();
    streamErrors->close();
  }

  void overrideNativeLogs() {
    customStdStreamLogs = std::make_shared<std::stringstream>();
    customStdStreamErrors = std::make_shared<std::stringstream>();

    nativeLogsCache = std::cout.rdbuf(customStdStreamLogs->rdbuf());
    nativeErrorsCache = std::cerr.rdbuf(customStdStreamErrors->rdbuf());
  }

  void revertNativeLogs() {
    std::cout.rdbuf(nativeLogsCache);
    std::cerr.rdbuf(nativeErrorsCache);

    if (!customStdStreamLogs->str().empty()) {
      write(customStdStreamLogs->str(), "log", true);
    }

    if (!customStdStreamErrors->str().empty()) {
      write(customStdStreamErrors->str(), "log", true);
    }
  }

  std::string generateId(int padding = 7) {
    auto now = std::chrono::high_resolution_clock::now().time_since_epoch();
    long millis =
        std::chrono::duration_cast<std::chrono::microseconds>(now).count();

    std::stringstream ss;
    ss << std::hex << millis;
    std::string result = ss.str();

    const char charset[] = "0123456789abcdef";
    const size_t max_index = (sizeof(charset) - 1);
    srand(time(NULL));
    while (padding > 0) {
      char random_hex = charset[rand() % max_index];
      std::stringstream ss2;
      ss2 << random_hex;

      result = result + ss2.str();
      padding = padding - 1;
    }

    return result;
  }
};
} // namespace runtime

#endif // CPP_RUNTIME_RUNTIMELOGGER_H
