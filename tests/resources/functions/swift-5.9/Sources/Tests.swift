import Foundation
import AsyncHTTPClient

func main(context: RuntimeContext) async throws -> RuntimeOutput {
    let action = context.req.headers["x-action"] ?? "default"

    switch action {
    case "plaintextResponse":
        return context.res.send("Hello World 👋")
    case "jsonResponse":
        return try context.res.json([
            "json": true,
            "message": "Developers are awesome."
        ])
    case "customCharsetResponse":
        return context.res.send("ÅÆ", headers: [
            "content-type": "text/plain; charset=iso-8859-1"
        ])
    case "multipartResponse":
        return context.res.send("""
--12345
Content-Disposition: form-data; name=\"partOne\"

Why just have one part?
--12345
Content-Disposition: form-data; name=\"partTwo\"

When you can have two!
--12345--
""", headers: [
            "content-type": "multipart/form-data; boundary=12345"
        ])
    case "redirectResponse":
        return context.res.redirect("https://github.com/")
    case "emptyResponse":
        return context.res.empty()
    case "noResponse":
        context.res.send("This should be ignored, as it is not returned.")

        // Simulate test data. Return necessary in Swift
        context.error("Return statement missing. return context.res.empty() if no response is expected.")

        return context.res.send("", statusCode: 500)
    case "doubleResponse":
        context.res.send("This should be ignored.")
        return context.res.send("This should be returned.")
    case "headersResponse":
        return context.res.send("OK", statusCode: 200, headers: [
            "first-header": "first-value",
            "second-header": context.req.headers["x-open-runtimes-custom-in-header"] ?? "missing",
            "cookie": context.req.headers["cookie"] ?? "missing",
            "x-open-runtimes-custom-out-header": "third-value"
        ])
    case "statusResponse":
        return context.res.send("FAIL", statusCode: 404)
    case "requestMethod":
        return context.res.send(context.req.method)
    case "requestHeadOrTrace":
        context.log(context.req.method)
        return context.res.send("", statusCode: 200)
    case "requestUrl":
        return try context.res.json([
            "url": context.req.url,
            "scheme": context.req.scheme,
            "host": context.req.host,
            "port": context.req.port,
            "path": context.req.path,
            "queryString": context.req.queryString,
            "query": context.req.query
        ])
    case "requestHeaders":
        return try context.res.json(context.req.headers)
    case "requestBodyPlaintext":
        return context.res.send(context.req.body as! String)
    case "requestBodyJson":
        var key1: String
        var key2: String

        if context.req.body is String {
            key1 = "Missing key"
            key2 = "Missing key"
        } else {
            let body = context.req.body as! [String: Any?]

            key1 = (body["key1"] as? String) ?? "Missing key"
            key2 = (body["key2"] as? String) ?? "Missing key"
        }

        return try context.res.json([
            "key1": key1,
            "key2": key2,
            "raw": context.req.bodyRaw
        ])
    case "envVars":
        return try context.res.json([
            "var": ProcessInfo.processInfo.environment["CUSTOM_ENV_VAR"],
            "emptyVar": ProcessInfo.processInfo.environment["NOT_DEFINED_VAR"]
        ])
    case "logs":
        context.log("Debug log")
        context.error("Error log")
                
        context.log("Log+With+Plus+Symbol")

        context.log(42)
        context.log(4.2)
        context.log(true)
        context.log(["objectKey": "objectValue"])
        context.log(["arrayValue"])

        // Swift doesn't support native log capturing
        context.log("Native log")
        context.log("Unsupported logs detected. Use context.log() or context.error() for logging.")

        return context.res.send("")
    case "library":
        let httpClient = HTTPClient(eventLoopGroupProvider: .createNew)
        let request = HTTPClientRequest(url: "https://jsonplaceholder.typicode.com/todos/\(context.req.bodyRaw)")
        let response = try await httpClient.execute(request, timeout: .seconds(30))
        let data = try await response.body.collect(upTo: 1024 * 1024)
        let todo = try JSONSerialization.jsonObject(with: data, options: []) as? [String: Any]

        return try context.res.json([
            "todo": todo
        ])
    case "timeout":
        context.log("Timeout start.")

        try await Task.sleep(nanoseconds: 3_000_000_000)
        try Task.checkCancellation()

        context.log("Timeout end.")
        return context.res.send("Successful response.")
    default:
        throw annotatedError(NSError(domain: "Unknown action", code: 500))
    }
}
