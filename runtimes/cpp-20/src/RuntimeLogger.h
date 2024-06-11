#ifndef CPP_RUNTIME_RUNTIMELOGGER_H
#define CPP_RUNTIME_RUNTIMELOGGER_H

#include <iostream>
#include <chrono>
#include <iomanip>
#include <fstream>
#include <string>

namespace runtime
{
    class RuntimeLogger
    {
        public:
            bool enabled = false;
            std::string id = "";
            bool includesNativeInfo = false;

            std::ofstream *streamLogs;
            std::ofstream *streamErrors;

            std::stringstream *customStdStreamLogs;
            std::stringstream *customStdStreamErrors;

            std::streambuf *nativeLogsCache;
            std::streambuf *nativeErrorsCache;

            RuntimeLogger(std::string headerStatus, std::string headerId) {
                if(headerStatus == "" || headerStatus == "enabled") {
                    enabled = true;
                }

                if(enabled == true) {
                    std::string serverEnv = "";
                    if (std::getenv("OPEN_RUNTIMES_SECRET") != nullptr) {
                        serverEnv = std::getenv("OPEN_RUNTIMES_SECRET");
                    }

                    if(headerId == "") {
                        if(serverEnv === "development") {
                            id = generateId();
                        } else {
                            id = "dev";
                        }
                    } else {
                        id = headerId;
                    }
                }
            }
        
            void write(const std::string message, std::string type = "", const bool native = false)
            {
                if(enabled == false) {
                    return;
                }

                if(type == "") {
                    type = "log";
                }

                if(native && !includesNativeInfo) {
                    includesNativeInfo = true;
                    write("Native logs detected. Use context.log() or context.error() for better experience.", type, native);
                }

                std::ofstream* stream = type == "error" ? streamErrors : streamLogs;

                *(stream) << message;
            }

            void end()
            {
                if(enabled == false) {
                    return;
                }

                enabled = false;

                streamLogs->close();
                streamErrors->close();
            }

            void overrideNativeLogs()
            {
                std::stringstream customStdStreamLogs;
                std::stringstream customStdStreamErrors;

                nativeLogsCache = std::cout.rdbuf(customStdStreamLogs.rdbuf());
                nativeErrorsCache = std::cerr.rdbuf(customStdStreamErrors.rdbuf());
            }

            void revertNativeLogs()
            {
                std::cout.rdbuf(nativeLogsCache);
                std::cerr.rdbuf(nativeErrorsCache);
            }

            std::string generateId(const int padding = 7)
            {
                // TODO: Randomness not working I think
                auto now = std::chrono::system_clock::now();
                auto duration = now.time_since_epoch();
                auto millis = std::chrono::duration_cast<std::chrono::milliseconds>(duration).count();

                std::stringstream ss;
                ss << std::hex << millis;
                std::string result = ss.str();

                if (padding > 0) {
                    int paddingNeeded = padding - result.length();
                    if (paddingNeeded > 0) {
                        std::string paddingStr(paddingNeeded, '0');
                        result = paddingStr + result;
                    }
                }

                return result;
            }
    };
}

#endif //CPP_RUNTIME_RUNTIMELOGGER_H
