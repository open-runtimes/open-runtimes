package io.openruntimes.kotlin;

class RuntimeOutput(body: String, statusCode: Int, headers: MutableMap<String, String>) {
    var body: String
    var statusCode: Int
    var headers: MutableMap<String, String>

    init {
        this.body = body
        this.statusCode = statusCode
        this.headers = headers
    }
}