package io.openruntimes.kotlin

public class RuntimeRequest(bodyString: String, body: Any, headers: MutableMap<String, String>, method: String, url: String) {
    var bodyString: String
    var body: Any
    var headers: MutableMap<String, String>
    var method: String
    var url: String

    init {
        this.bodyString = bodyString
        this.body = body
        this.headers = headers
        this.method = method
        this.url = url
    }
}