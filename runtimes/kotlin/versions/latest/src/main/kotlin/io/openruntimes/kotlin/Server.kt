package io.openruntimes.kotlin

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import com.google.gson.ToNumberPolicy
import io.javalin.Javalin
import io.javalin.http.Context
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeoutOrNull
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
    val serverSecret = System.getenv("OPEN_RUNTIMES_SECRET") ?: ""

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

    var enforcedHeadersString = System.getenv("OPEN_RUNTIMES_HEADERS")
    if (enforcedHeadersString == null || enforcedHeadersString.isEmpty()) {
        enforcedHeadersString = "{}"
    }
    val enforcedHeaders = gsonInternal.fromJson(enforcedHeadersString, MutableMap::class.java)

    for (entry in enforcedHeaders.entries.iterator()) {
        val header = "${entry.key}".lowercase()
        headers[header] = "${entry.value}"
    }

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

    var output: RuntimeOutput?

    try {
        var entrypoint = System.getenv("OPEN_RUNTIMES_ENTRYPOINT")
        entrypoint =
            entrypoint
                .substring(0, entrypoint.length - 3)
                .replace('/', '.')

        val classToLoad = Class.forName("io.openruntimes.kotlin.$entrypoint").kotlin
        val classMethod = classToLoad.memberFunctions.find { it.name == "main" }!!
        val instance = classToLoad.createInstance()

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

    resHeaders.forEach { (key, value) ->
        if (!key.startsWith("x-open-runtimes-")) {
            ctx.header(key, value)
        }
    }

    ctx.header("x-open-runtimes-log-id", logger.id ?: "")

    logger.end()

    ctx.status(output.statusCode).result(output.body)
}
