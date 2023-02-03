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
app.on(.HEAD, "", body: .stream, use: execute)
app.on(.HEAD, "**", body: .stream, use: execute)
app.on(.TRACE, "", body: .stream, use: execute)
app.on(.TRACE, "**", body: .stream, use: execute)

func execute(req: Request) async throws -> Response {
    if !req.headers.contains(name: "x-open-runtimes-secret")
        || req.headers["x-open-runtimes-secret"].first != ProcessInfo.processInfo.environment["OPEN_RUNTIMES_SECRET"] {
        return Response(
            status: .internalServerError,
            body: .init(string: "Unauthorized. Provide correct \"x-open-runtimes-secret\" header.")
        )
    }

    let rawBody = req.body.string ?? ""
    var body = rawBody as Any
    var headers = [String: String]()
    let method = req.method.string
    let url = req.url.path + (req.url.query.map { "?\($0)" } ?? "")

    for header in req.headers {
        let key = header.name.lowercased()
        if !key.starts(with: "x-open-runtimes-") {
            headers[key] = header.value
        }
    }

    let contentType = req.headers["content-type"].first ?? "text/plain"
    if contentType.starts(with: "application/json"),
        !rawBody.trimmingCharacters(in: CharacterSet.whitespacesAndNewlines).isEmpty,
        rawBody != "\"\"" {
            body = try JSONSerialization.jsonObject(
                with: rawBody.data(using: .utf8)!,
                options: .allowFragments
            ) as Any
    }

    let request = RuntimeRequest(
        rawBody: rawBody,
        body: body,
        headers: headers,
        method: method,
        url: url
    )
    
    let response = RuntimeResponse()
    
    let context = RuntimeContext(
        request: request,
        response: response
    )

    var output: RuntimeOutput

    do {
        output = try await main(context: context)
    } catch {
        context.error(message: error.localizedDescription)
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
