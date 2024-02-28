import Foundation

class RuntimeRequest {
    let bodyRaw: String
    let body: Any
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
        bodyRaw: String,
        body: Any,
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
        self.bodyRaw = bodyRaw
        self.body = body
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
}
