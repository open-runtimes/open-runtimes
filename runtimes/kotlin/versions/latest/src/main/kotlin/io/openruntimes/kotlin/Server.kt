package io.openruntimes.kotlin

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import com.google.gson.ToNumberPolicy
import io.javalin.Javalin
import io.javalin.http.Context
import kotlinx.coroutines.flow.Flow
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeoutOrNull
import org.eclipse.jetty.server.Request
import java.io.IOException
import java.io.PrintWriter
import java.io.StringWriter
import kotlin.reflect.full.callSuspend
import kotlin.reflect.full.createInstance
import kotlin.reflect.full.memberFunctions
import kotlin.time.Duration.Companion.seconds

val gson: Gson = GsonBuilder().serializeNulls().create()
val gsonInternal: Gson = GsonBuilder().serializeNulls().setObjectToNumberStrategy(ToNumberPolicy.LONG_OR_DOUBLE).create()

suspend fun main() {
    println("HTTP server successfully started!")

    val app =
        Javalin
            .create { config ->
                config.maxRequestSize = 20L * 1024 * 1024
            }.start(3000)
            .get("/*") { runBlocking { execute(it) } }
            .post("/*") { runBlocking { execute(it) } }
            .put("/*") { runBlocking { execute(it) } }
            .delete("/*") { runBlocking { execute(it) } }
            .patch("/*") { runBlocking { execute(it) } }
            .options("/*") { runBlocking { execute(it) } }
            .head("/*") { runBlocking { execute(it) } }
    app.jettyServer()!!.server().stopTimeout = 30_000
    Runtime.getRuntime().addShutdownHook(Thread { app.stop() })
}

suspend fun execute(ctx: Context) {
    if (ctx.path() == "/__opr/health") {
        ctx.status(200).result("OK")
        return
    }
    if (ctx.path() == "/__opr/timings") {
        val timings = java.io.File("/mnt/telemetry/timings.txt").readText()
        ctx.contentType("text/plain; charset=utf-8").result(timings)
        return
    }

    val logger = RuntimeLogger(ctx.header("x-open-runtimes-logging"), ctx.header("x-open-runtimes-log-id"))
    try {
        action(logger, ctx)
    } catch (e: Exception) {
        val sw = StringWriter()
        val pw = PrintWriter(sw)
        e.printStackTrace(pw)

        val message = sw.toString()

        ctx.header("x-open-runtimes-log-id", logger.id ?: "")

        logger.write(arrayOf(message), RuntimeLogger.TYPE_ERROR, false)
        logger.end()

        ctx.status(500).result("")
    }
}

suspend fun action(
    logger: RuntimeLogger,
    ctx: Context,
) {
    var safeTimeout = -1
    val timeout = ctx.header("x-open-runtimes-timeout") ?: ""
    if (timeout.isNotEmpty()) {
        var invalid = false
        try {
            safeTimeout = timeout.toInt()
        } catch (e: NumberFormatException) {
            invalid = true
        }

        if (invalid || safeTimeout < 0) {
            ctx.status(500).result("Header \"x-open-runtimes-timeout\" must be an integer greater than 0.")
            return
        }
    }

    val secret = ctx.header("x-open-runtimes-secret") ?: ""
    val serverSecret = OprConfig.secret

    if (serverSecret != "" && secret != serverSecret) {
        ctx.status(500).result("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.")
        return
    }

    val bodyBinary = ctx.bodyAsBytes()
    val headers = mutableMapOf<String, String>()
    val method = ctx.method()

    for (entry in ctx.headerMap().entries.iterator()) {
        val header = entry.key.lowercase()
        if (!(header.startsWith("x-open-runtimes-"))) {
            headers[header] = entry.value
        }
    }

    headers.putAll(OprConfig.headers)

    val hostHeader = ctx.header("host") ?: ""
    val protoHeader = ctx.header("x-forwarded-proto") ?: "http"

    val defaultPort = if (protoHeader == "https") "443" else "80"

    val host: String
    val port: Int

    if (hostHeader.contains(":")) {
        host = hostHeader.split(":")[0]
        port = hostHeader.split(":")[1].toInt()
    } else {
        host = hostHeader
        port = defaultPort.toInt()
    }

    val path = ctx.path()
    val queryString = ctx.queryString() ?: ""
    val query = HashMap<String, String>()

    for (param in queryString.split("&")) {
        val pair = param.split("=", limit = 2)

        if (pair.isNotEmpty() && pair[0].isNotEmpty()) {
            val value = if (pair.size == 2) pair[1] else ""
            query[pair[0]] = value
        }
    }

    var url = "$protoHeader://$host"

    if (port != defaultPort.toInt()) {
        url += ":$port"
    }

    url += path

    if (!queryString.isEmpty()) {
        url += "?$queryString"
    }

    val runtimeRequest =
        RuntimeRequest(
            method,
            protoHeader,
            host,
            port,
            path,
            query,
            queryString,
            headers,
            bodyBinary,
            url,
        )
    val runtimeResponse = RuntimeResponse()
    val context = RuntimeContext(runtimeRequest, runtimeResponse, logger)

    logger.overrideNativeLogs()

    var output: RuntimeOutput? = null
    var classMethod: kotlin.reflect.KFunction<*>? = null
    var instance: Any? = null

    val entrypoint = OprConfig.entrypoint

    // Guard: Try to load module
    try {
        val className =
            entrypoint
                .substring(0, entrypoint.length - 3)
                .replace('/', '.')

        val classToLoad = Class.forName("io.openruntimes.kotlin.$className").kotlin
        classMethod = classToLoad.memberFunctions.find { it.name == "main" }
        if (classMethod == null) {
            throw NoSuchMethodException("Function signature invalid. Did you forget to export a 'main' function?")
        }
        instance = classToLoad.createInstance()
    } catch (e: ClassNotFoundException) {
        context.error("Class not found: ${e.message}")
        logger.revertNativeLogs()
        output = context.res.text("", 503, mutableMapOf())
    } catch (e: NoSuchMethodException) {
        context.error(e.message ?: "Function signature invalid. Did you forget to export a 'main' function?")
        logger.revertNativeLogs()
        output = context.res.text("", 503, mutableMapOf())
    } catch (e: InstantiationException) {
        context.error("Failed to create instance: ${e.message}")
        logger.revertNativeLogs()
        output = context.res.text("", 503, mutableMapOf())
    } catch (e: IllegalAccessException) {
        context.error("Access denied: ${e.message}")
        logger.revertNativeLogs()
        output = context.res.text("", 503, mutableMapOf())
    } catch (e: Exception) {
        context.error("Failed to load module: ${e.message}")
        logger.revertNativeLogs()
        output = context.res.text("", 503, mutableMapOf())
    }

    // Execute user function
    if (output == null && classMethod != null && instance != null) {
        try {
            if (safeTimeout > 0) {
                output =
                    withTimeoutOrNull(safeTimeout.seconds) {
                        if (classMethod.isSuspend) {
                            classMethod.callSuspend(instance, context) as RuntimeOutput
                        } else {
                            classMethod.call(instance, context) as RuntimeOutput
                        }
                    }

                if (output == null) {
                    context.error("Execution timed out.")
                    output = context.res.text("", 500)
                }
            } else {
                output =
                    if (classMethod.isSuspend) {
                        classMethod.callSuspend(instance, context) as RuntimeOutput
                    } else {
                        classMethod.call(instance, context) as RuntimeOutput
                    }
            }
        } catch (e: Exception) {
            val sw = StringWriter()
            val pw = PrintWriter(sw)
            e.printStackTrace(pw)

            context.error(sw.toString())
            output = context.res.text("", 500, mutableMapOf())
        } finally {
            logger.revertNativeLogs()
        }
    }

    if (output == null) {
        context.error("Return statement missing. return context.res.empty() if no response is expected.")
        output = context.res.text("", 500, mutableMapOf())
    }

    val resHeaders = output.headers.mapKeys { it.key.lowercase() }.toMutableMap()

    if (resHeaders["content-type"] == null) {
        resHeaders["content-type"] = "text/plain"
    }

    if (!resHeaders["content-type"]!!.startsWith("multipart/")) {
        resHeaders["content-type"] = resHeaders["content-type"]!!.lowercase()

        if (!resHeaders["content-type"]!!.contains("charset=")) {
            resHeaders["content-type"] += "; charset=utf-8"
        }
    }

    // A streamed response is chunked, so its length is unknown.
    if (output.stream != null) {
        resHeaders.remove("content-length")
    }

    resHeaders.forEach { (key, value) ->
        if (!key.startsWith("x-open-runtimes-")) {
            ctx.header(key, value)
        }
    }

    ctx.header("x-open-runtimes-log-id", logger.id ?: "")

    if (output.stream != null) {
        // Status and headers leave with the first chunk, so they are final from here on.
        ctx.status(output.statusCode)
        stream(ctx, output.stream, context, logger, safeTimeout)
        return
    }

    logger.end()

    ctx.status(output.statusCode).result(output.body)
}

suspend fun stream(
    ctx: Context,
    source: Flow<Any>,
    context: RuntimeContext,
    logger: RuntimeLogger,
    safeTimeout: Int,
) {
    val out = ctx.res.outputStream
    val write: suspend () -> Unit = {
        source.collect { chunk ->
            val bytes = if (chunk is ByteArray) chunk else chunk.toString().toByteArray()
            if (bytes.isNotEmpty()) {
                out.write(bytes)
                out.flush()
            }
        }
    }

    var failure: String? = null
    logger.overrideNativeLogs()
    try {
        if (safeTimeout > 0) {
            if (withTimeoutOrNull(safeTimeout.seconds) { write() } == null) {
                failure = "Execution timed out."
            }
        } else {
            write()
        }
    } catch (e: Exception) {
        val sw = StringWriter()
        val pw = PrintWriter(sw)
        e.printStackTrace(pw)
        failure = sw.toString()
    } finally {
        logger.revertNativeLogs()
    }

    if (failure != null) {
        context.error(failure)
    }

    // Logs are read once the response finishes, so they are flushed first.
    logger.end()

    // Content is already on the wire, so a failure can only reach the caller as a truncation.
    if (failure != null) {
        Request.getBaseRequest(ctx.req).httpChannel.abort(IOException(failure))
    }
}
