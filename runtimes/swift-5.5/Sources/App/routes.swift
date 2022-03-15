import Vapor
import Foundation

struct RequestValue {
    var env: [String: String] = [:]
    var headers: [String: String] = [:]
    var payload: String = ""

    enum CodingKeys: String, CodingKey {
        case env
        case headers
        case payload
    }
}

extension RequestValue : Decodable {
    init(from decoder: Decoder) throws {
        let values = try decoder.container(keyedBy: CodingKeys.self)

        if let env = try? values.decodeIfPresent([String: String].self, forKey: .env) {
            self.env = env
        }
        if let headers = try? values.decodeIfPresent([String: String].self, forKey: .headers) {
            self.headers = headers
        }
        if let payload = try? values.decodeIfPresent(String.self, forKey: .payload) {
            self.payload = payload
        }
    }
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
            self.data = "Something went wrong encoding the Response JSON Object!"
            self.isJson = false
        }
        return self;
    }
    
    fileprivate static func error(from data: Error) -> RequestResponse {
        let response = RequestResponse()
        
        do {
            response.data = String(data: data.localizedDescription, encoding: String.Encoding.utf8)!
        } catch _ {
            response.data = "Something went wrong internally. Check the docker logs."
        }
            
        response.statusCode = HTTPResponseStatus.internalServerError
        response.isJson = false
        return response
    }
    
    fileprivate static func unauthorized() -> RequestResponse {
        let response = RequestResponse()
        response.statusCode = HTTPResponseStatus.internalServerError
        response.isJson = false
        response.data = "Unauthorized"
        return response
    }
}

extension RequestResponse: AsyncResponseEncodable {
    func encodeResponse(for request: Request) async throws -> Response {
        var headers = HTTPHeaders()
        
        switch self.isJson {
        case false:
            headers.add(name: .contentType, value: "text/plain")
        case true:
            headers.add(name: .contentType, value: "application/json")
        }

        return Response(
            status: self.statusCode, 
            headers: headers, 
            body: .init(string: self.data)
        )
    }
}

func routes(_ app: Application) throws {
    app.on(.POST, "", body: .stream) { req async throws -> RequestResponse in
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

            return try await main(req: request, res: response)
        } catch let error {
            return RequestResponse.error(from: error)
        }
    }
}
