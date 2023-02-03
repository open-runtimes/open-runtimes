import Foundation

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
    case "redirectResponse":
        return context.res.redirect("https://github.com/")
    case "emptyResponse":
        return context.res.empty()
    case "noResponse":
        context.res.send("This should be ignored, as it is not returned.")

        // Simulate test data. Return necessary in Swift
        context.error(message: "Return statement missing. return context.res.empty() if no response is expected.")
        return context.res.send("", statusCode: 500)
    case "doubleResponse":
        context.res.send("This should be ignored.")
        return context.res.send("This should be returned.")
    case "headersResponse":
        return context.res.send("OK", statusCode: 200, headers: [
            "first-header": "first-value",
            "second-header": context.req.headers["x-open-runtimes-custom-in-header"] ?? "missing",
            "x-open-runtimes-custom-out-header": "third-value"
        ])
    case "statusResponse":
        return context.res.send("FAIL", statusCode: 404)
    case "requestMethod":
        return context.res.send(context.req.method)
    case "requestUrl":
        return context.res.send(context.req.url)
    case "requestHeaders":
        return try context.res.json(context.req.headers)
    case "requestBodyPlaintext":
        return context.res.send(context.req.body as! String)
    case "requestBodyJson":
        var key1: String
        var key2: String

        if let string = context.req.body as? String {
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
            "raw": context.req.rawBody
        ])
    case "envVars":
        return try context.res.json([
            "var": ProcessInfo.processInfo.environment["CUSTOM_ENV_VAR"],
            "emptyVar": ProcessInfo.processInfo.environment["NOT_DEFINED_VAR"]
        ])
    case "logs":
        context.log(message: "Debug log")
        context.error(message: "Error log")

        context.log(message: 42)
        context.log(message: 4.2)
        context.log(message: true)

        // Swift doesn't support native log capturing
        context.log(message: "Unsupported log noticed. Use context.log() or context.error() for logging.")

        return context.res.send("")
    default:
        throw NSError(domain: "Unknown action", code: 500)
    }
}
