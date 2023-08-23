package io.openruntimes.kotlin

import com.google.gson.GsonBuilder

public class RuntimeResponse {
    companion object {
        private val gson = GsonBuilder().serializeNulls().create()
    }

    fun send(
        body: String,
        statusCode: Int = 200,
        headers: MutableMap<String, String> = mutableMapOf()
    ): RuntimeOutput {
        if (!headers.containsKey("content-type")) {
            headers["content-type"] = "text/plain"
        }
        return RuntimeOutput(body, statusCode, headers)
    }

    fun json(
        json: MutableMap<String, Any>,
        statusCode: Int = 200,
        headers: MutableMap<String, String> = mutableMapOf()
    ): RuntimeOutput {
        if (!headers.containsKey("content-type")) {
            headers["content-type"] = "application/json"
        }
        return this.send(gson.toJson(json), statusCode, headers)
    }

    fun empty(): RuntimeOutput {
        return RuntimeOutput("", 204, mutableMapOf<String, String>())
    }

    fun redirect(
        url: String,
        statusCode: Int = 301,
        headers: MutableMap<String, String> = mutableMapOf()
    ): RuntimeOutput {
        headers["location"] = url
        return this.send("", statusCode, headers)
    }
}