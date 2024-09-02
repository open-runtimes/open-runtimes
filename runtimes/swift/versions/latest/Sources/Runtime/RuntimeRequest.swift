import Foundation

class RuntimeRequest {
    let bodyBinary: Data
    let headers: [String: String]
    let method: String
    let url: String
    let scheme: String?
    let host: String
    let port: Int?
    let path: String
    let queryString: String?
    let query: [String: String]?

    init(
        bodyBinary: Data,
        headers: [String: String],
        method: String,
        url: String,
        scheme: String?,
        host: String,
        port: Int?,
        path: String,
        queryString: String?,
        query: [String: String]?
    ) {
        self.bodyBinary = bodyBinary
        self.headers = headers
        self.method = method
        self.url = url
        self.scheme = scheme
        self.host = host
        self.port = port
        self.path = path
        self.queryString = queryString
        self.query = query
    }

    var bodyText: String {
        if let text = String(data: bodyBinary, encoding: .utf8) {
            return text
        } else {
            return ""
        }
    }

    var bodyJson: [String: Any?] {
        if !bodyText.trimmingCharacters(in: CharacterSet.whitespacesAndNewlines).isEmpty,
           bodyText != "\"\""
        {
            do {
                return try JSONSerialization.jsonObject(
                    with: bodyText.data(using: .utf8)!,
                    options: .allowFragments
                ) as! [String: Any?]
            } catch {
                return [String: Any?]()
            }
        } else {
            return [String: Any?]()
        }
    }

    var bodyRaw: String {
        bodyText
    }

    var body: Any {
        let contentType = (headers["content-type"] ?? "text/plain").lowercased()

        if contentType.hasPrefix("application/json") {
            return bodyJson
        }

        return bodyText
    }
}
