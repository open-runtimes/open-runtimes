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

func main(req: RequestValue, res: RequestResponse) -> RequestResponse {
    print(req)
    
    let payload = try! JSONSerialization.jsonObject(with: req.payload!.data(using: .utf8)!, options: []) as! [String: Any]
    let todoId = payload["id"] as! Int

    let headerData = req.headers!["x-test-header"]
    let envData = req.env!["test-env"]

    let todo = (try! JSONSerialization.jsonObject(
        with: (try! URLRequest(url: URL(string: "https://jsonplaceholder.typicode.com/todos/\(todoId)")!)).httpBody!, 
        options: []) as! [String: Any]
    )

    return res.json(data: [
        "isTest": true,
        "message": "Hello Open Runtimes 👋",
        "todo": todo,
        "header": headerData,
        "env": envData
    ])
}