import Foundation
import AsyncHTTPClient

func main(context: RuntimeContext) async throws -> RuntimeOutput {
    let payload = context.req.body as? [String: Any]
    let id = (payload?["id"] as? String) ?? "1"

    let httpClient = HTTPClient(eventLoopGroupProvider: .createNew)
    let request = HTTPClientRequest(url: "https://jsonplaceholder.typicode.com/todos/\(id)")
    let response = try await httpClient.execute(request, timeout: .seconds(30))
    let data = try await response.body.collect(upTo: 1024 * 1024)
    let todo = try JSONSerialization.jsonObject(with: data, options: []) as? [String: Any]

    return try context.res.json([
        "message": "Hello Open Runtimes 👋",
        "todo": todo
    ])
}