import Foundation

struct RuntimeOutput {
    let body: Data
    let statusCode: Int
    let headers: [String: String]
    var stream: (@Sendable (@escaping @Sendable (Data) async -> Bool) async throws -> Void)?
}
