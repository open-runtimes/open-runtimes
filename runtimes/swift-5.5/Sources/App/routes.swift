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
    var data: Any = ""
    var statusCode: HTTPResponseStatus = HTTPResponseStatus.ok

    init (data: String = "") {
        self.data = data
    }
}

extension RequestResponse {
    func send(data: String) -> RequestResponse {
        self.data = data
        self.statusCode = HTTPResponseStatus.ok
        return self
    }
    
    func json(data: [String: Any?]) -> RequestResponse {
        self.data = data
        self.statusCode = HTTPResponseStatus.ok
        return self
    }
    
    fileprivate static func error(from data: Error) -> RequestResponse {
        let response = RequestResponse()
        response.data = data.localizedDescription
        response.statusCode = HTTPResponseStatus.internalServerError
        return response
    }
    
    fileprivate static func unauthorized() -> RequestResponse {
        let response = RequestResponse()
        response.statusCode = HTTPResponseStatus.internalServerError
        response.data = "Unauthorized"
        return response
    }
}

extension RequestResponse: AsyncResponseEncodable {
    func encodeResponse(for request: Request) async throws -> Response {
        var headers = HTTPHeaders()
        headers.add(name: .contentType, value: "application/json")

        if let JSONData = try? JSONSerialization.data(
          withJSONObject: self.data,
          options: .prettyPrinted
        ), let JSONText = String(data: JSONData, encoding: String.Encoding.utf8) {
            self.statusCode = HTTPResponseStatus.ok
            self.data = JSONText
        } else {
            self.statusCode = HTTPResponseStatus.internalServerError
            self.data = "Something went wrong encoding the Response JSON Object!"
        }

        return Response(
            status: self.statusCode, 
            headers: headers, 
            body: .init(string: self.data as! String)
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

        setvbuf(stdout, nil, _IONBF, 0)
        setvbuf(stderr, nil, _IONBF, 0)

        let outPipe = Pipe()
        let errPipe = Pipe()
        var outString = ""
        var errString = ""

        do {
            var request = RequestValue() 
            let response = RequestResponse()

            outPipe.fileHandleForReading.readabilityHandler = { fileHandle in
                let data = fileHandle.availableData
                if let string = String(data: data, encoding: String.Encoding.utf8) {
                    outString += string
                }
            }
            errPipe.fileHandleForReading.readabilityHandler = { fileHandle in
                let data = fileHandle.availableData
                if let string = String(data: data, encoding: String.Encoding.utf8) {
                    errString += string
                }
            }

            dup2(outPipe.fileHandleForWriting.fileDescriptor, STDOUT_FILENO)
            dup2(errPipe.fileHandleForWriting.fileDescriptor, STDERR_FILENO)

            if let body = req.body.string,
                !body.trimmingCharacters(in: CharacterSet.whitespacesAndNewlines).isEmpty,
                body != "\"\"" {
                    request = try JSONDecoder().decode(RequestValue.self, from: body.data(using: .utf8)!)
            }

            let userResponse = try await main(req: request, res: response)
            var output = [String:Any?]()
            output["response"] = userResponse.data
            output["stdout"] = outString
            return userResponse.json(data: output)
        } catch let error {
            errString += error.localizedDescription
            var output = [String:Any?]()
            output["stdout"] = outString
            output["stderr"] = errString
            return RequestResponse().json(data: output)
        }
    }
}
