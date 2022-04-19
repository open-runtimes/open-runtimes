package io.openruntimes.kotlin

import io.javalin.http.Context

class RuntimeRequest(ctx: Context) {
    var payload = ""
    var headers = mutableMapOf<String, String>()
    var env=  mutableMapOf<String, String>()

    init {
        val data = ctx.bodyAsClass<Map<String, Any>>()
        payload = data["payload"] as? String ?: ""
        headers = data["headers"] as? MutableMap<String, String> ?: mutableMapOf()
        env = data["env"] as? MutableMap<String, String> ?: mutableMapOf()
    }
}