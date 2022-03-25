package io.openruntimes.kotlin

import io.javalin.plugin.json.JsonMapper

class RuntimeResponse(
    var data: String = "",
    var statusCode: Int = 200
) {
    fun json(data: Map<String, Any?>, statusCode: Int = 200): RuntimeResponse {
        this.data = mapper?.toJsonString(data) ?: ""
        this.statusCode = statusCode
        return this
    }

    fun send(data: String, statusCode: Int = 200): RuntimeResponse {
        this.data = data
        this.statusCode = statusCode
        return this
    }

    companion object {
        var mapper: JsonMapper? = null

        @JvmStatic
        fun error(error: Throwable): RuntimeResponse {
            return RuntimeResponse("{\"message\": \"" + error.message + "\"}", 500)
        }

        @JvmStatic
        fun unauthorized(): RuntimeResponse {
            return RuntimeResponse("{\"message\": \"Unauthorized\"}", 401)
        }
    }
}