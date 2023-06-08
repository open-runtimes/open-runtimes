package io.openruntimes.kotlin

import io.javalin.Javalin
import io.javalin.http.Context
import io.javalin.plugin.json.jsonMapper
import java.io.ByteArrayOutputStream
import java.io.StringWriter
import java.io.PrintStream
import java.io.PrintWriter
import kotlinx.coroutines.runBlocking
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.io.UnsupportedEncodingException
import com.google.gson.GsonBuilder
import com.google.gson.Gson
import java.lang.reflect.Method
import java.util.HashMap

val gson = GsonBuilder().serializeNulls().create()

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
}

suspend fun execute(ctx: Context) {
    val secret = ctx.header("x-open-runtimes-secret") ?: ""
    val serverSecret = System.getenv("OPEN_RUNTIMES_SECRET") ?: ""

    if (secret == "" || secret != serverSecret) {
        ctx.status(500).result("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.")
        return
    }

    val bodyString = ctx.body()
    var body = bodyString as Any
    val headers = mutableMapOf<String, String>()
    val method = ctx.method()

    for (entry in ctx.headerMap().entries.iterator()) {
        val header = entry.key.lowercase()
        if(!(header.startsWith("x-open-runtimes-"))) {
            headers.put(header, entry.value)
        }
    }

    val contentType = ctx.header("content-type") ?: "text/plain"
    if(contentType.contains("application/json")) {
        if(!bodyString.isEmpty()) {
            body = gson.fromJson(bodyString, MutableMap::class.java)
        } else {
            body = HashMap<String, Any>()
        }
    }

    val hostHeader = ctx.header("host") ?: ""
    val protoHeader = ctx.header("x-forwarded-proto") ?: "http"

    val scheme = protoHeader
    val defaultPort = if (scheme == "https") "443" else "80"

    var host: String
    var port: Int

    if(hostHeader.contains(":")) {
        host = hostHeader.split(":")[0]
        port = hostHeader.split(":")[1].toInt()
    } else {
        host = hostHeader
        port = defaultPort.toInt()
    }

    val path = ctx.path()
    val queryString = ctx.queryString() ?: ""
    val query = HashMap<String, String>()

    for(param in queryString.split("&")) {
        val pair = param.split("=", limit = 2)

        if(pair.size >= 1 && !pair[0].isEmpty()) {
            val value = if (pair.size == 2) pair[1] else ""
            query.put(pair[0], value)
        }
    }

    var url = scheme + "://" + host

    if(port != defaultPort.toInt()) {
        url += ":$port"
    }

    url += path

    if(!queryString.isEmpty()) {
        url += "?" + queryString
    }

    val runtimeRequest = RuntimeRequest(url, method, scheme, host, port, path, query, queryString, headers, body, bodyString)
    val runtimeResponse = RuntimeResponse()
    val context = RuntimeContext(runtimeRequest, runtimeResponse)

    val systemOut = System.out
    val systemErr = System.err

    val customstdStream = ByteArrayOutputStream()
    val customstd = PrintStream(customstdStream)
    System.setOut(customstd)
    System.setErr(customstd)

    var output: RuntimeOutput?

    try {
        var entrypoint = System.getenv("OPEN_RUNTIMES_ENTRYPOINT")
        entrypoint = entrypoint.substring(0, entrypoint.length - 3); // Remove .kt
        entrypoint = entrypoint.replace('/', '.')

        val classToLoad = Class.forName("io.openruntimes.kotlin." + entrypoint)
        val classMethod = classToLoad.getDeclaredMethod("main", RuntimeContext::class.java)
        val instance = classToLoad.newInstance()
        output = classMethod.invoke(instance, context) as RuntimeOutput
    } catch (e: Exception) {
        val sw = StringWriter()
        val pw = PrintWriter(sw)
        e.printStackTrace(pw)

        context.error(sw.toString())
        output = context.res.send("", 500, mutableMapOf<String, String>())
    } finally {
        System.out.flush()
        System.err.flush()
        System.setOut(systemOut)
        System.setErr(systemErr)
    }

    if(output == null) {
        context.error("Return statement missing. return context.res.empty() if no response is expected.")
        output = context.res.send("", 500, mutableMapOf<String, String>())
    }

    for ((key, value) in output.headers) {
        val header = key.lowercase()
        if(!(header.startsWith("x-open-runtimes-"))) {
            ctx.header(header, value)
        }
    }

    if(!customstdStream.toString().isEmpty()) {
        context.log("Unsupported log detected. Use context.log() or context.error() for logging.")
    }

    try {
        ctx.header("x-open-runtimes-logs", URLEncoder.encode(context.logs.joinToString("\n"), StandardCharsets.UTF_8.toString()))
        ctx.header("x-open-runtimes-errors", URLEncoder.encode(context.errors.joinToString("\n"), StandardCharsets.UTF_8.toString()))
    } catch (ex: UnsupportedEncodingException) {
        ctx.header("x-open-runtimes-logs", "Internal error while processing logs.")
        ctx.header("x-open-runtimes-errors", "Internal error while processing logs.")
    }

    ctx.status(output.statusCode).result(output.body)
}