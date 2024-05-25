package io.openruntimes.kotlin

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import io.javalin.Javalin
import io.javalin.http.Context
import io.javalin.http.HandlerType
import kotlinx.coroutines.runBlocking
import kotlinx.coroutines.withTimeoutOrNull
import java.io.*
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import kotlin.reflect.KClass
import kotlin.reflect.full.callSuspend
import kotlin.reflect.full.createInstance
import kotlin.reflect.full.memberFunctions
import kotlin.time.Duration.Companion.seconds

val gson: Gson = GsonBuilder().serializeNulls().create()

suspend fun main() {
    Javalin
        .create()
        .start(3000)
        .get("/*") { runBlocking { execute(it) } }
        .post("/*") { runBlocking { execute(it) } }
        .put("/*") { runBlocking { execute(it) } }
        .delete("/*") { runBlocking { execute(it) } }
        .patch("/*") { runBlocking { execute(it) } }
        .options("/*") { runBlocking { execute(it) } }
        .head("/*") { runBlocking { execute(it) } }
        .addHandler(HandlerType.TRACE, "/*") { runBlocking { execute(it) } }
}

suspend fun execute(ctx: Context) {
    try {
        action(ctx)
    } catch (e: Exception) {
        val logs = mutableListOf<String>()
        val errors = mutableListOf<String>()

        val sw = StringWriter()
        val pw = PrintWriter(sw)
        e.printStackTrace(pw)

        errors.add(sw.toString())

        try {
            ctx.header(
                "x-open-runtimes-logs",
                URLEncoder.encode(logs.joinToString("\n"), StandardCharsets.UTF_8.toString())
            )
            ctx.header(
                "x-open-runtimes-errors",
                URLEncoder.encode(errors.joinToString("\n"), StandardCharsets.UTF_8.toString())
            )
        } catch (ex: UnsupportedEncodingException) {
            ctx.header("x-open-runtimes-logs", "Internal error while processing logs.")
            ctx.header("x-open-runtimes-errors", "Internal error while processing logs.")
        }
    
        ctx.status(500).result("")
    }
}

suspend fun action(ctx: Context) {
    var safeTimeout = -1;
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

    if (secret == "" || secret != serverSecret) {
        ctx.status(500).result("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.")
        return
    }

    val bodyRaw = ctx.body()
    var body = bodyRaw as Any
    val headers = mutableMapOf<String, String>()
    val method = ctx.method()

    for (entry in ctx.headerMap().entries.iterator()) {
        val header = entry.key.lowercase()
        if (!(header.startsWith("x-open-runtimes-"))) {
            headers[header] = entry.value
        }
    }

    val contentType = ctx.header("content-type") ?: "text/plain"
    if (contentType.contains("application/json")) {
        body = if (bodyRaw.isNotEmpty()) {
            gson.fromJson(bodyRaw, MutableMap::class.java)
        } else {
            mutableMapOf<String, Any>()
        }
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

    val runtimeRequest = RuntimeRequest(
        method,
        protoHeader,
        host,
        port,
        path,
        query,
        queryString,
        headers,
        body,
        bodyRaw,
        url,
    )
    val runtimeResponse = RuntimeResponse()
    val context = RuntimeContext(runtimeRequest, runtimeResponse)

    val systemOut = System.out
    val systemErr = System.err

    val customStdStream = ByteArrayOutputStream()
    val customStd = PrintStream(customStdStream)
    System.setOut(customStd)
    System.setErr(customStd)

    var output: RuntimeOutput?

    try {
        var entrypoint = System.getenv("OPEN_RUNTIMES_ENTRYPOINT")
        entrypoint = entrypoint
            .substring(0, entrypoint.length - 3)
            .replace('/', '.')

        val classToLoad = Class.forName("io.openruntimes.kotlin.$entrypoint").kotlin
        val classMethod = classToLoad.memberFunctions.find { it.name == "main" }!!
        val instance = classToLoad.createInstance()

        if (safeTimeout > 0) {
            output = withTimeoutOrNull(safeTimeout.seconds) {
                if (classMethod.isSuspend) {
                    classMethod.callSuspend(instance, context) as RuntimeOutput
                } else {
                    classMethod.call(instance, context) as RuntimeOutput
                }
            }

            if (output == null) {
                context.error("Execution timed out.")
                output = context.res.send("", 500)
            }
        } else {
            output = if (classMethod.isSuspend) {
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
        output = context.res.send("", 500, mutableMapOf())
    } finally {
        System.out.flush()
        System.err.flush()
        System.setOut(systemOut)
        System.setErr(systemErr)
    }

    if (output == null) {
        context.error("Return statement missing. return context.res.empty() if no response is expected.")
        output = context.res.send("", 500, mutableMapOf())
    }

    val resHeaders = output.headers.mapKeys { it.key.lowercase() }.toMutableMap();

    if (resHeaders["content-type"] == null) {
        resHeaders["content-type"] = "text/plain"
    }

    if (!resHeaders["content-type"]!!.startsWith("multipart/") && !resHeaders["content-type"]!!.contains("charset=")) {
        resHeaders["content-type"] += "; charset=utf-8"
    }

    resHeaders.forEach { (key, value) ->
        if (!key.startsWith("x-open-runtimes-")) {
            ctx.header(key, value)
        }
    }

    if (customStdStream.toString().isNotEmpty()) {
        context.log("");
        context.log("----------------------------------------------------------------------------");
        context.log("Unsupported logs detected. Use context.log() or context.error() for logging.");
        context.log("----------------------------------------------------------------------------");
        context.log(customStdStream.toString());
        context.log("----------------------------------------------------------------------------");
    }

    try {
        ctx.header(
            "x-open-runtimes-logs",
            URLEncoder.encode(context.logs.joinToString("\n"), StandardCharsets.UTF_8.toString())
        )
        ctx.header(
            "x-open-runtimes-errors",
            URLEncoder.encode(context.errors.joinToString("\n"), StandardCharsets.UTF_8.toString())
        )
    } catch (ex: UnsupportedEncodingException) {
        ctx.header("x-open-runtimes-logs", "Internal error while processing logs.")
        ctx.header("x-open-runtimes-errors", "Internal error while processing logs.")
    }

    ctx.status(output.statusCode).result(output.body)
}