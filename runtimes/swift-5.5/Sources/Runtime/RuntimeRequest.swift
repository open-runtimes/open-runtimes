import Foundation

class RuntimeRequest {
    var rawBody: String
    var body: Any
    var headers: [String: String]
    var method: String
    var url: String

    init(rawBody: String, body: Any, headers: [String: String], method: String, url: String) {
        self.rawBody = rawBody
        self.body = body
        self.headers = headers
        self.method = method
        self.url = url
    }
}
