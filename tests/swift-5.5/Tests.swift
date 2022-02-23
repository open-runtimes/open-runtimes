import Foundation
#if canImport(FoundationNetworking)
import FoundationNetworking
#endif

import Collections

//    'req' variable has:
//        'headers' - object with request headers
//        'payload' - object with request body data
//        'env' - object with environment variables
//    'res' variable has:
//        'send(text, status)' - function to return text response. Status code defaults to 200
//        'json(obj, status)' - function to return JSON response. Status code defaults to 200
//    
//    If an error is thrown, a response with code 500 will be returned.

func main(req: RequestValue, res: RequestResponse) throws -> RequestResponse {

    let headerData = req.headers["x-test-header"]
    let envData = req.env["test-env"]

    var todoId: String = "1"
    var todo: [String: Any]? = nil

    if !req.payload.isEmpty,
        let data = req.payload.data(using: .utf8),
        let payload = try? JSONSerialization.jsonObject(with: data, options: []) as? [String: Any] {
            todoId = payload["id"] as! String
    }

    let group = DispatchGroup()
    let url = URL(string: "https://jsonplaceholder.typicode.com/todos/\(todoId)")!
    let task = URLSession.shared.dataTask(with: url) { (data, response, error) in
        defer { group.leave() }
        guard let data = data else { return }
        todo = try! JSONSerialization.jsonObject(with: data, options: []) as? [String: Any]
    }
    group.enter()
    task.resume()
    group.wait()

    return res.json(data: [
        "isTest": true,
        "message": "Hello Open Runtimes 👋",
        "todo": todo,
        "header": headerData,
        "env": envData
    ])
}
