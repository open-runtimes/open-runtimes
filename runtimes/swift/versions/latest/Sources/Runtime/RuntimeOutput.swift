import Foundation

class RuntimeOutput {
    let body: Data
    let statusCode: Int
    let headers: [String: String]

    init(body: Data, statusCode: Int, headers: [String: String]) {
        self.body = body
        self.statusCode = statusCode
        self.headers = headers
    }
}