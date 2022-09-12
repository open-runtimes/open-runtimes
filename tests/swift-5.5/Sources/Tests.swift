import Foundation
import AsyncHTTPClient

//    'req' variable has:
//        'headers' - object with request headers
//        'payload' - object with request body data
//        'variables' - object with function variables
//    'res' variable has:
//        'send(text, status)' - function to return text response. Status code defaults to 200
//        'json(obj, status)' - function to return JSON response. Status code defaults to 200
//    
//    If an error is thrown, a response with code 500 will be returned.

func main(req: RequestValue, res: RequestResponse) async throws -> RequestResponse {

    let headerData = req.headers["x-test-header"]
    let varData = req.variables["test-variable"]

    var todoId: String = "1"

    var reqPayload = req.payload as! String
    if(reqPayload == "") {
        reqPayload = "{}"
    }

    if !reqPayload.isEmpty,
        let data = reqPayload.data(using: .utf8),
        let payload = try? JSONSerialization.jsonObject(with: data, options: []) as? [String: Any] {
            todoId = payload["id"] as? String ?? "1"
    }

    let httpClient = HTTPClient(eventLoopGroupProvider: .createNew)
    let request = HTTPClientRequest(url: "https://jsonplaceholder.typicode.com/todos/\(todoId)")
    let response = try await httpClient.execute(request, timeout: .seconds(30))
    let data = try await response.body.collect(upTo: 1024 * 1024)
    let todo = try JSONSerialization.jsonObject(with: data, options: []) as? [String: Any]

    print("log1")
    print("{hello: world}")
    print("[hello, world]")

    return res.json(data: [
        "isTest": true,
        "message": "Hello Open Runtimes 👋",
        "todo": todo,
        "header": headerData,
        "variable": varData
    ])
}
