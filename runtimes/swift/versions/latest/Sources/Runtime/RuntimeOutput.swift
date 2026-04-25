import Foundation

struct RuntimeOutput {
    let body: Data
    let statusCode: Int
    let headers: [String: String]
}
