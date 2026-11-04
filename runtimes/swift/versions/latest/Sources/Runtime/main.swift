import Foundation
import Vapor

var env = try Environment.detect()
try LoggingSystem.bootstrap(from: &env)
let app = Application(env)
defer { app.shutdown() }

app.on(.GET, "", body: .collect(maxSize: "20mb"), use: execute)
app.on(.GET, "**", body: .collect(maxSize: "20mb"), use: execute)
app.on(.POST, "", body: .collect(maxSize: "20mb"), use: execute)
app.on(.POST, "**", body: .collect(maxSize: "20mb"), use: execute)
app.on(.PATCH, "", body: .collect(maxSize: "20mb"), use: execute)
app.on(.PATCH, "**", body: .collect(maxSize: "20mb"), use: execute)
app.on(.PUT, "", body: .collect(maxSize: "20mb"), use: execute)
app.on(.PUT, "**", body: .collect(maxSize: "20mb"), use: execute)
app.on(.DELETE, "", body: .collect(maxSize: "20mb"), use: execute)
app.on(.DELETE, "**", body: .collect(maxSize: "20mb"), use: execute)
app.on(.OPTIONS, "", body: .collect(maxSize: "20mb"), use: execute)
app.on(.OPTIONS, "**", body: .collect(maxSize: "20mb"), use: execute)

func execute(req: Request) async throws -> Response {
    if req.uri.path == "/__opr/health" {
        return Response(
            status: .ok,
            body: .init(string: "OK")
        )
    }

    if req.uri.path == "/__opr/timings" {
        let timings = try String(contentsOfFile: "/mnt/telemetry/timings.txt", encoding: .utf8)
        var outputHeaders = HTTPHeaders()
        outputHeaders.add(name: "content-type", value: "text/plain; charset=utf-8")
        return Response(
            status: .ok,
            headers: outputHeaders,
            body: .init(string: timings)
        )
    }

    let headerLogger = req.headers["x-open-runtimes-logging"]
    var loggerStatus = ""
    if !headerLogger.isEmpty {
        loggerStatus = headerLogger.first ?? ""
    }

    let headerLogId = req.headers["x-open-runtimes-log-id"]
    var logId = ""
    if !headerLogId.isEmpty {
        logId = headerLogId.first ?? ""
    }

    // A HEAD response has no body to carry them, so its logs are dropped
    let logger = RuntimeLogger(
        status: loggerStatus,
        id: logId,
        inline: OprConfig.inlineLogs && req.method != .HEAD
    )
    do {
        return try await action(logger: logger, req: req)
    } catch {
        logger.write(message: error, type: RuntimeLogger.TYPE_ERROR)
        logger.end()

        var outputHeaders = HTTPHeaders()
        outputHeaders.add(name: "x-open-runtimes-log-id", value: logger.id)

        return respond(logger: logger, statusCode: 500, headers: outputHeaders, body: Data())
    }
}

/// With OPEN_RUNTIMES_LOGGING=inline, the body goes out in a `b` frame and the
/// logs and errors follow in `l` and `e` frames, so edge splits them off the
/// body. Each frame is its type, the payload's byte length in hex and a
/// newline, then the payload.
func frame(_ type: String, _ bytes: Data) -> Data {
    if bytes.isEmpty {
        return Data()
    }
    var out = Data("\(type)\(String(bytes.count, radix: 16))\n".utf8)
    out.append(bytes)
    return out
}

func respond(logger: RuntimeLogger, statusCode: Int, headers: HTTPHeaders, body: Data) -> Response {
    var statusCode = statusCode
    var headers = headers
    var body = body

    if logger.inline {
        // Framing gives the body a length of its own and needs a status that allows
        // a body, so the real ones travel in headers
        headers.remove(name: "content-length")
        headers.replaceOrAdd(name: "x-open-runtimes-logs-inline", value: "enabled")
        headers.replaceOrAdd(name: "x-open-runtimes-content-length", value: String(body.count))
        if statusCode == 204 || statusCode == 304 {
            headers.replaceOrAdd(name: "x-open-runtimes-status-code", value: String(statusCode))
            statusCode = 200
        }
        body = frame("b", body) + frame("l", logger.inlineLogs) + frame("e", logger.inlineErrors)
    }

    return Response(
        status: .custom(code: UInt(statusCode), reasonPhrase: ""),
        headers: headers,
        body: .init(data: body)
    )
}

func action(logger: RuntimeLogger, req: Request) async throws -> Response {
    var safeTimeout = -1
    let timeout = req.headers["x-open-runtimes-timeout"]
    if !timeout.isEmpty {
        let timeoutInt = Int(timeout.first!) ?? 0
        if timeoutInt == 0 {
            return Response(
                status: .internalServerError,
                body: .init(
                    string: "Header \"x-open-runtimes-timeout\" must be an integer greater than 0."
                )
            )
        }

        safeTimeout = timeoutInt
    }

    if OprConfig.secret != "" {
        if !req.headers.contains(name: "x-open-runtimes-secret")
            || req.headers["x-open-runtimes-secret"].first != OprConfig.secret
        {
            return Response(
                status: .internalServerError,
                body: .init(
                    string: "Unauthorized. Provide correct \"x-open-runtimes-secret\" header."
                )
            )
        }
    }

    var headers = [String: String]()
    let method = req.method.string
    let scheme = req.uri.scheme!
    let host = req.uri.host!
    let port = req.uri.port!
    let path = req.uri.path
    let queryString = req.uri.query
    var query = [String: String]()

    if let queryString {
        for param in queryString.split(separator: "&") {
            let parts = param.split(separator: "=", maxSplits: 1)

            var key: String? = nil
            var value: String? = nil

            if parts.isEmpty {
                continue
            }
            if parts.count >= 1 {
                key = String(parts[0])
            }
            if parts.count == 2 {
                value = String(parts[1])
            }

            query[key!] = value ?? ""
        }
    }

    var url = "\(scheme)://\(host)"

    if (scheme == "http" && port != 80) || (scheme == "https" && port != 443) {
        url += ":\(port)"
    }

    url += path

    if !((queryString ?? "").isEmpty) {
        url += "?"
        url += queryString!
    }

    for header in req.headers {
        let key = header.name.lowercased()
        if !key.starts(with: "x-open-runtimes-") {
            headers[key] = header.value
        }
    }

    for (key, value) in OprConfig.headers {
        headers[key] = value
    }

    var bodyBinary = Data()
    if let requestBodyData = req.body.data {
        bodyBinary = Data(buffer: requestBodyData)
    }

    let request = RuntimeRequest(
        bodyBinary: bodyBinary,
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
        response: response,
        logger: logger
    )

    var output: RuntimeOutput

    do {
        if safeTimeout > 0 {
            do {
                output = try await withThrowingTaskGroup(of: RuntimeOutput.self) { group in
                    let deadline = Date(timeIntervalSinceNow: Double(safeTimeout))

                    group.addTask {
                        // swiftformat:disable:next hoistAwait, hoistTry
                        try await annotateError(try await main(context: context))
                    }
                    group.addTask {
                        let interval = deadline.timeIntervalSinceNow
                        try await Task.sleep(nanoseconds: UInt64(interval * 1_000_000_000))
                        try Task.checkCancellation()
                        throw CancellationError()
                    }

                    let result = try await group.next()!

                    group.cancelAll()

                    return result
                }
            } catch {
                context.error("Execution timed out.")
                output = context.res.text("", statusCode: 500)
            }
        } else {
            // swiftformat:disable:next hoistAwait, hoistTry
            output = try await annotateError(try await main(context: context))
        }
    } catch {
        context.error(error)
        output = context.res.text("", statusCode: 500)
    }

    var outputHeaders = HTTPHeaders()
    for header in output.headers {
        let key = header.key.lowercased()
        if !key.starts(with: "x-open-runtimes-") {
            outputHeaders.add(name: key, value: header.value)
        }
    }

    let contentTypeValue = (outputHeaders.first(name: "content-type") ?? "text/plain").lowercased()
    if !contentTypeValue.starts(with: "multipart/"), !contentTypeValue.contains("charset=") {
        outputHeaders.replaceOrAdd(
            name: "content-type", value: contentTypeValue + "; charset=utf-8"
        )
    }

    logger.end()
    outputHeaders.add(name: "x-open-runtimes-log-id", value: logger.id)

    return respond(logger: logger, statusCode: output.statusCode, headers: outputHeaders, body: output.body)
}

print("HTTP server successfully started!")

try app.run()
