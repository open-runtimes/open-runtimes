import Vapor
import Foundation

struct RequestValue: Codable {
    var path: String? = "/usr/code"
    var file: String? = "index.swift"
    var env: [String: String]? = [:]
    var headers: [String: String]? = [:]
    var payload: String? = ""
}

class RequestResponse {
    var data: String = ""
    var statusCode: HTTPResponseStatus = HTTPResponseStatus.ok
    private var isJson: Bool = false

    init (data: String = "") {
        self.data = data
    }
}

extension RequestResponse {
    func send(data: String) -> RequestResponse {
        self.isJson = false
        self.data = data
        self.statusCode = HTTPResponseStatus.ok
        return self
    }
    
    func json(data: [String: Any?]) -> RequestResponse {
        if let JSONData = try? JSONSerialization.data(
          withJSONObject: data,
          options: .prettyPrinted
        ), let JSONText = String(data: JSONData, encoding: String.Encoding.utf8) {
            self.statusCode = HTTPResponseStatus.ok
            self.data = JSONText
            self.isJson = true
        } else {
            self.statusCode = HTTPResponseStatus.internalServerError;
            self.data = "{\"code\": 500, \"message\": \"Something went wrong encoding the Response JSON Object!\"}"
            self.isJson = true
        }
        return self;
    }
    
    fileprivate static func error(from data: Error) -> RequestResponse {
        let response = RequestResponse()
        var jsonObject = [String:Any]()
        jsonObject["code"] = 500
        jsonObject["message"] = data.localizedDescription
        
        do {
            let jsonData = try JSONSerialization.data(withJSONObject: jsonObject, options: [])
            let jsonString = String(data: jsonData, encoding: String.Encoding.utf8)!
            response.data = jsonString
        } catch _ {
            response.data = "{\"code\": 500, \"message\": \"Something went wrong internally. Check the docker logs.\"}"
        }
            
        response.statusCode = HTTPResponseStatus.internalServerError
        response.isJson = true
        return response
    }
    
    fileprivate static func unauthorized() -> RequestResponse {
        let response = RequestResponse()
        response.statusCode = HTTPResponseStatus.unauthorized
        response.isJson = true
        response.data = "{\"code\": 401, \"message\": \"Unauthorized\"}"
        return response
    }
}

extension RequestResponse: ResponseEncodable {
    func encodeResponse(for request: Request) -> EventLoopFuture<Response> {
        var headers = HTTPHeaders()
        
        switch self.isJson {
        case false:
            headers.add(name: .contentType, value: "text/plain")
        case true:
            headers.add(name: .contentType, value: "application/json")
        }
        
        return request.eventLoop.makeSucceededFuture(.init(
            status: self.statusCode, headers: headers, body: .init(string: self.data)
        ))
    }
}

func routes(_ app: Application) throws {
    app.on(.POST, "", body: .stream) { req -> RequestResponse in
        if (!req.headers.contains(name: "x-internal-challenge") || req.headers["x-internal-challenge"].isEmpty) {
            return RequestResponse.unauthorized()
        }

        if (req.headers["x-internal-challenge"][0] != ProcessInfo.processInfo.environment["INTERNAL_RUNTIME_KEY"]) {
            return RequestResponse.unauthorized()
        }

        do {
            var request = RequestValue() 
            let response = RequestResponse()

            if let body = req.body.string, 
                !body.trimmingCharacters(in: CharacterSet.whitespacesAndNewlines).isEmpty,
                body != "\"\"" {
                    request = try JSONDecoder().decode(RequestValue.self, from: body.data(using: .utf8)!)
            }

            return try main(req: request, res: response)
        } catch let error {
            return RequestResponse.error(from: error)
        }
    }
}
