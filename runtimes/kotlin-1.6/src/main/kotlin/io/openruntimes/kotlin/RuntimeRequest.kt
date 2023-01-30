package io.openruntimes.kotlin

public class RuntimeRequest(rawBody: String, body: Any, headers: MutableMap<String, String>, method: String, url: String) {
    var rawBody: String
    var body: Any
    var headers: MutableMap<String, String>
    var method: String
    var url: String

    init {
        this.rawBody = rawBody
        this.body = body
        this.headers = headers
        this.method = method
        this.url = url
    }
}