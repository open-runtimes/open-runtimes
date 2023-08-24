import Foundation

class RuntimeResponse {

    func send(
        _ body: String,
        statusCode: Int = 200,
        headers: [String: String] = [:]
    ) -> RuntimeOutput {
        return RuntimeOutput(
            body: body,
            statusCode: statusCode,
            headers: headers
        )
    }

    func json(
        _ json: [String: Any?],
        statusCode: Int = 200,
        headers: [String: String] = [:]
    ) throws -> RuntimeOutput {
        var outputHeaders = headers
        
        if outputHeaders["content-type"] == nil {
            outputHeaders["content-type"] = "application/json"
        }
        
        let data = try JSONSerialization.data(
          withJSONObject: json,
          options: .prettyPrinted
        )

        let text = String(
            data: data,
            encoding: .utf8
        )

        return RuntimeOutput(
            body: text!,
            statusCode: statusCode,
            headers: outputHeaders
        )
    }

    func empty() -> RuntimeOutput {
        return RuntimeOutput(
            body: "",
            statusCode: 204,
            headers: [:]
        )
    }

    func redirect(
        _ url: String,
        statusCode: Int = 301,
        headers: [String: String] = [:]
    ) -> RuntimeOutput {
        var outputHeaders = headers

        outputHeaders["location"] = url

        return RuntimeOutput(
            body: "",
            statusCode: statusCode,
            headers: outputHeaders
        )
    }
}
