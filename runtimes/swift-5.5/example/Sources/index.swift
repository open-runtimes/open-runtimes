//
//  File.swift
//  
//
//  Created by Bradley Schofield on 21/12/2021.
//

import Foundation
import AsyncHTTPClient

func main(req: RequestValue, res: RequestResponse) async throws -> RequestResponse {
    var todoId: String = "1"

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
        "message": "Hello Open Runtimes 👋",
        "todo": todo
    ])
}
