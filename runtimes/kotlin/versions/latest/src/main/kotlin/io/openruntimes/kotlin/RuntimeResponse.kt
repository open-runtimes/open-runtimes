package io.openruntimes.kotlin

import com.google.gson.GsonBuilder
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.flow.asFlow

public class RuntimeResponse {
    companion object {
        private val gson = GsonBuilder().serializeNulls().create()
    }

    fun binary(
        bytes: ByteArray,
        statusCode: Int = 200,
        headers: Map<String, String> = mapOf(),
    ): RuntimeOutput = RuntimeOutput(bytes, statusCode, headers)

    fun text(
        body: String,
        statusCode: Int = 200,
        headers: Map<String, String> = mapOf(),
    ): RuntimeOutput = binary(body.toByteArray(), statusCode, headers)

    fun send(
        body: String,
        statusCode: Int = 200,
        headers: Map<String, String> = mapOf(),
    ): RuntimeOutput = text(body, statusCode, headers)

    fun json(
        json: MutableMap<String, Any>,
        statusCode: Int = 200,
        headers: MutableMap<String, String> = mutableMapOf(),
    ): RuntimeOutput {
        headers["content-type"] = "application/json"
        return this.text(gson.toJson(json), statusCode, headers)
    }

    fun empty(): RuntimeOutput = this.text("", 204, mutableMapOf<String, String>())

    fun redirect(
        url: String,
        statusCode: Int = 301,
        headers: MutableMap<String, String> = mutableMapOf(),
    ): RuntimeOutput {
        headers["location"] = url
        return this.text("", statusCode, headers)
    }

    fun stream(
        source: Flow<Any>,
        statusCode: Int = 200,
        headers: Map<String, String> = mapOf(),
    ): RuntimeOutput = RuntimeOutput(ByteArray(0), statusCode, headers, source)

    fun stream(
        source: Sequence<Any>,
        statusCode: Int = 200,
        headers: Map<String, String> = mapOf(),
    ): RuntimeOutput = stream(source.asFlow(), statusCode, headers)
}
