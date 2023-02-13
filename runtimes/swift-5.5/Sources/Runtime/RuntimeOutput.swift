import Foundation

class RuntimeOutput {
    let body: String
    let statusCode: Int
    let headers: [String: String]

    init(body: String, statusCode: Int, headers: [String: String]) {
        self.body = body
        self.statusCode = statusCode
        self.headers = headers
    }
}
