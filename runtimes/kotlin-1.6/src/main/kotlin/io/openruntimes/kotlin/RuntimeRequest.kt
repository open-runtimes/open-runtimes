package io.openruntimes.kotlin

public class RuntimeRequest(bodyString: String, body: Any, headers: MutableMap<String, String>, method: String, url: String, path: String, host: String, scheme: String, port: Int, queryString: String, query: MutableMap<String, String>) {
    var bodyString: String
    var body: Any
    var headers: MutableMap<String, String>
    var method: String
    var url: String
    var path: String
    var host: String
    var scheme: String
    var port: Int
    var queryString: String
    var query: MutableMap<String, String>

    init {
        this.bodyString = bodyString
        this.body = body
        this.headers = headers
        this.method = method
        this.url = url
        this.path = path
        this.host = host
        this.scheme = scheme
        this.port = port
        this.queryString = queryString
        this.query = query
    }
}