import Foundation

struct RuntimeOutput: Sendable {
    let body: Data
    let statusCode: Int
    let headers: [String: String]
}
