import Foundation
import AsyncHTTPClient

//    'req' variable has:
//        'headers' - object with request headers
//        'payload' - object with request body data
//        'env' - object with environment variables
//    'res' variable has:
//        'send(text, status)' - function to return text response. Status code defaults to 200
//        'json(obj, status)' - function to return JSON response. Status code defaults to 200
//    
//    If an error is thrown, a response with code 500 will be returned.

func main(req: RequestValue, res: RequestResponse) async throws -> RequestResponse {

    let headerData = req.headers["x-test-header"]
    let envData = req.env["test-env"]

    var todoId: String = "1"

    if(req.payload.isEmpty) {
        req.payload = "{}"
    }

    if !req.payload.isEmpty,
        let data = req.payload.data(using: .utf8),
        let payload = try? JSONSerialization.jsonObject(with: data, options: []) as? [String: Any] {
            todoId = payload["id"] as? String ?? "1"
    }

    let httpClient = HTTPClient(eventLoopGroupProvider: .createNew)
    let request = HTTPClientRequest(url: "https://jsonplaceholder.typicode.com/todos/\(todoId)")
    let response = try await httpClient.execute(request, timeout: .seconds(30))
    let data = try await response.body.collect(upTo: 1024 * 1024)
    let todo = try JSONSerialization.jsonObject(with: data, options: []) as? [String: Any]

    return res.json(data: [
        "isTest": true,
        "message": "Hello Open Runtimes 👋",
        "todo": todo,
        "header": headerData,
        "env": envData
    ])
}
