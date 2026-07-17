import Foundation

enum OprConfig {
    static let secret = ProcessInfo.processInfo.environment["OPEN_RUNTIMES_SECRET"] ?? ""
    static let env = ProcessInfo.processInfo.environment["OPEN_RUNTIMES_ENV"] ?? ""

    static let headers: [String: String] = {
        guard
            let serverHeadersString = ProcessInfo.processInfo.environment["OPEN_RUNTIMES_HEADERS"],
            serverHeadersString != "",
            let data = serverHeadersString.data(using: .utf8),
            let serverHeaders = (try? JSONSerialization.jsonObject(
                with: data,
                options: .allowFragments
            )) as? [String: Any?]
        else {
            return [:]
        }

        var headers = [String: String]()
        for (key, value) in serverHeaders {
            if let value {
                headers[key.lowercased()] = String(describing: value)
            } else {
                headers[key.lowercased()] = ""
            }
        }
        return headers
    }()
}
