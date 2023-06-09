import Vapor
import Foundation

var env = try Environment.detect()
try LoggingSystem.bootstrap(from: &env)
let app = Application(env)

app.on(.GET, "", body: .stream, use: execute)
app.on(.GET, "**", body: .stream, use: execute)
app.on(.POST, "", body: .stream, use: execute)
app.on(.POST, "**", body: .stream, use: execute)
app.on(.PATCH, "", body: .stream, use: execute)
app.on(.PATCH, "**", body: .stream, use: execute)
app.on(.PUT, "", body: .stream, use: execute)
app.on(.PUT, "**", body: .stream, use: execute)
app.on(.DELETE, "", body: .stream, use: execute)
app.on(.DELETE, "**", body: .stream, use: execute)
app.on(.OPTIONS, "", body: .stream, use: execute)
app.on(.OPTIONS, "**", body: .stream, use: execute)

func execute(req: Request) async throws -> Response {
    if !req.headers.contains(name: "x-open-runtimes-secret")
        || req.headers["x-open-runtimes-secret"].first != ProcessInfo.processInfo.environment["OPEN_RUNTIMES_SECRET"] {
        return Response(
            status: .internalServerError,
            body: .init(string: "Unauthorized. Provide correct \"x-open-runtimes-secret\" header.")
        )
    }

    let bodyString = req.body.string ?? ""
    var body = bodyString as Any
    var headers = [String: String]()
    let method = req.method.string
    let url = req.url.string
    let scheme = req.url.scheme
    let host = app.http.server.configuration.hostname
    let port = app.http.server.configuration.port
    let path = req.url.path
    let queryString = req.url.query
    var query = [String: String]()
    
    if let queryString = queryString {
        for param in queryString.split(separator: "&") {
            let pair = param.split(separator: "=", maxSplits: 1)
            let key = String(pair[0])
            let value = String(pair[1])
            
            query[key] = value
        }
    }
    
    for header in req.headers {
        let key = header.name.lowercased()
        if !key.starts(with: "x-open-runtimes-") {
            headers[key] = header.value
        }
    }

    let contentType = req.headers["content-type"].first ?? "text/plain"
    if contentType.starts(with: "application/json"),
        !bodyString.trimmingCharacters(in: CharacterSet.whitespacesAndNewlines).isEmpty,
        bodyString != "\"\"" {
            body = try JSONSerialization.jsonObject(
                with: bodyString.data(using: .utf8)!,
                options: .allowFragments
            ) as Any
    }

    let request = RuntimeRequest(
        bodyString: bodyString,
        body: body,
        headers: headers,
        method: method,
        url: url,
        scheme: scheme,
        host: host,
        port: port,
        path: path,
        queryString: queryString,
        query: query
    )
    
    let response = RuntimeResponse()
    
    let context = RuntimeContext(
        request: request,
        response: response
    )

    var output: RuntimeOutput

    do {
        output = try await annotateError(try await main(context: context))
    } catch {
        context.error(error)
        output = context.res.send("", statusCode: 500)
    }

    var outputHeaders = HTTPHeaders()
    for header in output.headers {
        let key = header.key.lowercased()
        if !key.starts(with: "x-open-runtimes-") {
            outputHeaders.add(name: key, value: header.value)
        }
    }

    var logs = context.logs.joined(separator: "\n")
    logs = logs.addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed) ?? logs

    var errors = context.errors.joined(separator: "\n")
    errors = errors.addingPercentEncoding(withAllowedCharacters: .urlQueryAllowed) ?? errors

    outputHeaders.add(name: "x-open-runtimes-logs", value: logs)
    outputHeaders.add(name: "x-open-runtimes-errors", value: errors)

    return Response(
        status: .custom(code: UInt(output.statusCode), reasonPhrase: ""),
        headers: outputHeaders,
        body: .init(string: output.body)
    )
}

try app.run()
