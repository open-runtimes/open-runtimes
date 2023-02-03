import Foundation

class RuntimeOutput {
    var body: String
    var statusCode: Int
    var headers: [String: String]

    init(body: String, statusCode: Int, headers: [String: String]) {
        self.body = body
        self.statusCode = statusCode
        self.headers = headers
    }
}