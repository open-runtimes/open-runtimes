import Foundation

class RuntimeLogger {
    static let TYPE_LOG = "log"
    static let TYPE_ERROR = "error"

    var enabled = false
    var id = ""

    var streamLogs: FileHandle?
    var streamErrors: FileHandle?

    init(status: String, id: String) {
        if status == "enabled" || status == "" {
            enabled = true
        }

        if enabled {
            if id == "" {
                let serverEnv = ProcessInfo.processInfo.environment["OPEN_RUNTIMES_ENV"]
                if serverEnv == "development" {
                    self.id = "dev"
                } else {
                    self.id = generateId()
                }
            } else {
                self.id = id
            }

            let logsUrl = URL(fileURLWithPath: "/mnt/logs/" + self.id + "_logs.log")
            let errorsUrl = URL(fileURLWithPath: "/mnt/logs/" + self.id + "_errors.log")

            if !FileManager.default.fileExists(atPath: logsUrl.path) {
                FileManager.default.createFile(atPath: logsUrl.path, contents: nil, attributes: nil)
            }

            if !FileManager.default.fileExists(atPath: errorsUrl.path) {
                FileManager.default.createFile(atPath: errorsUrl.path, contents: nil, attributes: nil)
            }

            do {
            streamLogs = try FileHandle(forWritingTo: logsUrl)
            streamErrors = try FileHandle(forWritingTo: errorsUrl)

                if let stream = streamLogs {
                    stream.seekToEndOfFile()
                }

                if let stream = streamErrors {
                    stream.seekToEndOfFile()
                }
            } catch {
                // Silently fail to prevent 500 errors in runtime
                // Log write failures should not crash the runtime
            }
        }
    }

    func write(message: Any, type: String = RuntimeLogger.TYPE_LOG) {
        if enabled == false {
            return
        }

        var stream = streamLogs

        if type == RuntimeLogger.TYPE_ERROR {
            stream = streamErrors
        }

        var stringLog = ""
        if message is CollectionType {
            if let data = try? JSONSerialization.data(withJSONObject: message),
               let stringTemp = String(data: data, encoding: .utf8)
            {
                stringLog = stringTemp
            }
        }

        if stringLog == "" {
            stringLog = String(describing: message)
        }

        stringLog += "\n"

        if let stringLogTemp = stringLog.data(using: .utf8) {
            if let streamTemp = stream {
                streamTemp.write(stringLogTemp)
            }
        }
    }

    func end() {
        if !enabled {
            return
        }

        enabled = false

        if let stream = streamLogs {
            stream.closeFile()
        }

        if let stream = streamErrors {
            stream.closeFile()
        }
    }

    func generateId(padding: Int = 7) -> String {
        let now = Date()
        let secs = Int(now.timeIntervalSince1970)
        let usec = Int((now.timeIntervalSince1970 - Double(secs)) * 1_000_000)
        let baseId = String(format: "%08x%05x", secs, usec)
        let randomPadding = (1 ... padding).map {
            _ in String(format: "%x", Int.random(in: 0 ..< 16))
        }.joined()
        return baseId + randomPadding
    }
}
