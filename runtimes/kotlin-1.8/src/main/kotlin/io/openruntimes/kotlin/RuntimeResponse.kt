package io.openruntimes.kotlin

import com.google.gson.GsonBuilder
import javax.servlet.http.HttpServletResponse


public class RuntimeResponse(
    val res: HttpServletResponse,
    val logger: RuntimeLogger
) {
    companion object {
        private val gson = GsonBuilder().serializeNulls().create()
    }

    var chunkHeaderSent = false

    fun binary(
        body: ByteArray,
        statusCode: Int = 200,
        headers: Map<String, String> = mapOf()
    ): RuntimeOutput {
        return RuntimeOutput(body, statusCode, headers, false)
    }

    fun text(
        body: String,
        statusCode: Int = 200,
        headers: Map<String, String> = mapOf()
    ): RuntimeOutput {
        return binary(body.toByteArray(), statusCode, headers)
    }

    fun send(
        body: String,
        statusCode: Int = 200,
        headers: Map<String, String> = mapOf()
    ): RuntimeOutput {
        return text(body, statusCode, headers)
    }

    fun json(
        json: MutableMap<String, Any>,
        statusCode: Int = 200,
        headers: MutableMap<String, String> = mutableMapOf()
    ): RuntimeOutput {
        headers["content-type"] = "application/json"
        return this.text(gson.toJson(json), statusCode, headers)
    }

    fun empty(): RuntimeOutput {
        return this.text("", 204, mutableMapOf<String, String>())
    }

    fun redirect(
        url: String,
        statusCode: Int = 301,
        headers: MutableMap<String, String> = mutableMapOf()
    ): RuntimeOutput {
        headers["location"] = url
        return this.text("", statusCode, headers)
    }

    fun start(statusCode: Int = 200, headers: MutableMap<String, String> = mutableMapOf()) {
        if(!chunkHeaderSent) {
            this.chunkHeaderSent = true

            headers.putIfAbsent("cache-control", "no-store")
            headers.putIfAbsent("content-type", "text/event-stream")
            headers.putIfAbsent("connection", "keep-alive")
            headers.putIfAbsent("transfer-encoding", "chunked")
            headers.putIfAbsent("x-open-runtimes-log-id", logger.id ?: "")

            for ((key, value) in headers.entries.iterator()) {
                res.setHeader(key, value)
            }

            res.status = statusCode
            res.outputStream.flush()
        }else{
            throw Exception("You can only call res.start() once")
        }
    }

    @Throws(java.lang.Exception::class)
    fun writeJson(body: Any) {
        this.writeText(gson.toJson(body))
    }

    @Throws(java.lang.Exception::class)
    fun writeText(body: String) {
        this.writeBinary(body.toByteArray())
    }

    @Throws(java.lang.Exception::class)
    fun writeBinary(body: ByteArray) {
        if (!this.chunkHeaderSent) {
            throw java.lang.Exception("You must call res.start() to start a chunk response")
        }

        res.outputStream.write(body)
        res.outputStream.flush()
    }

    @Throws(java.lang.Exception::class)
    fun end(): RuntimeOutput {
        if (!this.chunkHeaderSent) {
            throw java.lang.Exception("You must call res.start() to start a chunk response")
        }

        return RuntimeOutput("".toByteArray(), 0, mapOf(), true)
    }
}
