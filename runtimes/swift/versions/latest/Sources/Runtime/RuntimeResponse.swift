import Foundation

class RuntimeResponse {
    func binary(
        _ bytes: Data,
        statusCode: Int = 200,
        headers: [String: String] = [:]
    ) -> RuntimeOutput {
        RuntimeOutput(
            body: bytes,
            statusCode: statusCode,
            headers: headers
        )
    }

    func text(
        _ body: String,
        statusCode: Int = 200,
        headers: [String: String] = [:]
    ) -> RuntimeOutput {
        if let binaryBody = body.data(using: .utf8) {
            return binary(binaryBody, statusCode: statusCode, headers: headers)
        } else {
            return binary(Data(), statusCode: statusCode, headers: headers)
        }
    }

    func send(
        _ body: String,
        statusCode: Int = 200,
        headers: [String: String] = [:]
    ) -> RuntimeOutput {
        text(body, statusCode: statusCode, headers: headers)
    }

    func json(
        _ json: [String: Any?],
        statusCode: Int = 200,
        headers: [String: String] = [:]
    ) throws -> RuntimeOutput {
        var outputHeaders = headers

        outputHeaders["content-type"] = "application/json"

        let data = try JSONSerialization.data(
            withJSONObject: json
        )

        let text = String(
            data: data,
            encoding: .utf8
        )

        return self.text(text!, statusCode: statusCode, headers: outputHeaders)
    }

    func empty() -> RuntimeOutput {
        text("", statusCode: 204)
    }

    func redirect(
        _ url: String,
        statusCode: Int = 301,
        headers: [String: String] = [:]
    ) -> RuntimeOutput {
        var outputHeaders = headers

        outputHeaders["location"] = url

        return text("", statusCode: statusCode, headers: outputHeaders)
    }

    func sendStream<S: AsyncSequence & Sendable>(
        _ source: S,
        statusCode: Int = 200,
        headers: [String: String] = [:]
    ) -> RuntimeOutput where S.Element == Data {
        let iterator = StreamIterator(source.makeAsyncIterator())
        return RuntimeOutput(
            body: Data(),
            statusCode: statusCode,
            headers: headers,
            stream: { try await iterator.next() }
        )
    }

    func sendStream<S: AsyncSequence & Sendable>(
        _ source: S,
        statusCode: Int = 200,
        headers: [String: String] = [:]
    ) -> RuntimeOutput where S.Element: StringProtocol & Sendable {
        sendStream(source.map { Data($0.utf8) }, statusCode: statusCode, headers: headers)
    }
}

/// Async iterators are not Sendable; the runtime pulls from one task at a time.
final class StreamIterator<Iterator: AsyncIteratorProtocol>: @unchecked Sendable {
    var iterator: Iterator

    init(_ iterator: Iterator) {
        self.iterator = iterator
    }

    func next() async throws -> Iterator.Element? {
        try await iterator.next()
    }
}
