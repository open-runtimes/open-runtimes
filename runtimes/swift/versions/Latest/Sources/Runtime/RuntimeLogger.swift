import Foundation

class RuntimeLogger {
    static let TYPE_LOG = "log"
    static let TYPE_ERROR = "error"

    var enabled = false
    var id = ""

    var streamLogs: FileHandle? = nil
    var streamErrors: FileHandle? = nil

    init(status: String, id: String) {
        if(status == "enabled" || status == "") {
            self.enabled = true
        }

        if(self.enabled) {
            if(id == "") {
                let serverEnv = ProcessInfo.processInfo.environment["OPEN_RUNTIMES_ENV"]
                if(serverEnv == "development") {
                    self.id = "dev"
                } else {
                    self.id = self.generateId()
                }
            } else {
                self.id = id
            }

            let logsUrl = URL(fileURLWithPath:"/mnt/logs/" + self.id + "_logs.log")
            let errorsUrl = URL(fileURLWithPath:"/mnt/logs/" + self.id + "_errors.log")

            if !FileManager.default.fileExists(atPath: logsUrl.path) {
                FileManager.default.createFile(atPath: logsUrl.path, contents: nil, attributes: nil)
            }

            if !FileManager.default.fileExists(atPath: errorsUrl.path) {
                FileManager.default.createFile(atPath: errorsUrl.path, contents: nil, attributes: nil)
            }

            self.streamLogs = try! FileHandle(forWritingTo: logsUrl)
            self.streamErrors = try! FileHandle(forWritingTo: errorsUrl)

            if let stream = self.streamLogs {
                stream.seekToEndOfFile()
            }

            if let stream = self.streamErrors {
                stream.seekToEndOfFile()
            }
        }
    }

    func write(message: Any, type: String = RuntimeLogger.TYPE_LOG) {
        if(self.enabled == false) {
            return
        }

        var stream = self.streamLogs;

        if(type == RuntimeLogger.TYPE_ERROR) {
            stream = self.streamErrors;
        }

        var stringLog = ""
        if message is CollectionType {
            if let data = try? JSONSerialization.data(withJSONObject: message),
                let stringTemp = String(data: data, encoding: .utf8) {
                    stringLog = stringTemp
            }
        }
        
        if(stringLog == "") {
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
        if(!self.enabled) {
            return;
        }

        self.enabled = false;

        if let stream = self.streamLogs {
            stream.closeFile()
        }

        if let stream = self.streamErrors {
            stream.closeFile()
        }
    }

    func generateId(padding: Int = 7) -> String {
        let now = Date()
        let secs = Int(now.timeIntervalSince1970)
        let usec = Int((now.timeIntervalSince1970 - Double(secs)) * 1_000_000)
        let baseId = String(format: "%08x%05x", secs, usec)
        let randomPadding = (1...padding).map {
            _ in String(format: "%x", Int.random(in: 0..<16))
        }.joined()
        return baseId + randomPadding
    }
}
