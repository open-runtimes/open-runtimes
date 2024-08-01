import Foundation
import Vapor

var env = try Environment.detect()
try LoggingSystem.bootstrap(from: &env)
let app = Application(env)

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

    let logger = RuntimeLogger(status: loggerStatus, id: logId)
    do {
        return try await action(logger: logger, req: req)
    } catch {
        logger.write(message: error, type: RuntimeLogger.TYPE_ERROR)
        logger.end()

        var outputHeaders = HTTPHeaders()
        outputHeaders.add(name: "x-open-runtimes-log-id", value: logger.id)

        let code: HTTPResponseStatus = .custom(code: UInt(500), reasonPhrase: "")
        let resBody: Response.Body = .init(string: "")

        return Response(
            status: code,
            headers: outputHeaders,
            body: resBody
        )
    }
}

func action(logger: RuntimeLogger, req: Request) async throws -> Response {
    var safeTimeout = -1
    let timeout = req.headers["x-open-runtimes-timeout"]
    if !timeout.isEmpty {
        let timeoutInt = Int(timeout.first!) ?? 0
        if timeoutInt == 0 {
            return Response(
                status: .internalServerError,
                body: .init(string: "Header \"x-open-runtimes-timeout\" must be an integer greater than 0.")
            )
        }

        safeTimeout = timeoutInt
    }

    if let serverSecret = ProcessInfo.processInfo.environment["OPEN_RUNTIMES_SECRET"] {
        if serverSecret != "" {
            if !req.headers.contains(name: "x-open-runtimes-secret") || req.headers["x-open-runtimes-secret"].first != serverSecret {
                return Response(
                    status: .internalServerError,
                    body: .init(string: "Unauthorized. Provide correct \"x-open-runtimes-secret\" header.")
                )
            }
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

    if var serverHeadersString = ProcessInfo.processInfo.environment["OPEN_RUNTIMES_HEADERS"] {
        if serverHeadersString == "" {
            serverHeadersString = "{}"
        }

        let serverHeaders = try JSONSerialization.jsonObject(
            with: serverHeadersString.data(using: .utf8)!,
            options: .allowFragments
        ) as! [String: Any?]

        for (key, value) in serverHeaders {
            if let value {
                headers[key.lowercased()] = String(describing: value)
            } else {
                headers[key.lowercased()] = ""
            }
        }
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
        outputHeaders.replaceOrAdd(name: "content-type", value: contentTypeValue + "; charset=utf-8")
    }

    logger.end()
    outputHeaders.add(name: "x-open-runtimes-log-id", value: logger.id)

    let code: HTTPResponseStatus = .custom(code: UInt(output.statusCode), reasonPhrase: "")
    let resBody: Response.Body = .init(data: output.body)

    return Response(
        status: code,
        headers: outputHeaders,
        body: resBody
    )
}

print("HTTP server successfully started!")

try app.run()
