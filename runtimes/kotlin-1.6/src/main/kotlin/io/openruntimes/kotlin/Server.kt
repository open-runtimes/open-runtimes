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
    if (ctx.header("x-open-runtimes-secret").isNullOrBlank() || ctx.header("x-open-runtimes-secret") != System.getenv("OPEN_RUNTIMES_SECRET")) {
        ctx.status(500).result("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.")
        return
    }

    var rawBody: String = ctx.body()
    var body: Any = rawBody
    var headers: MutableMap<String, String> = mutableMapOf<String, String>()
    var method: String = ctx.method()
    var url: String = ctx.path()

    if(!ctx.queryString().isNullOrEmpty()) {
        url += "?" + ctx.queryString()
    }

    for (entry in ctx.headerMap().entries.iterator()) {
        var header: String = entry.key.lowercase()
        if(!(header.startsWith("x-open-runtimes-"))) {
            headers.put(header, entry.value)
        }
    }

    var contentType: String = ctx.header("content-type") ?: "text/plain"
    if(contentType.contains("application/json")) {
        var gson = GsonBuilder().serializeNulls().create();
        body = gson.fromJson(rawBody, MutableMap::class.java)
    }

    var runtimeRequest: RuntimeRequest = RuntimeRequest(rawBody, body, headers, method, url)
    var runtimeResponse: RuntimeResponse = RuntimeResponse()
    var context: RuntimeContext = RuntimeContext(runtimeRequest, runtimeResponse)

    var customstdStream: ByteArrayOutputStream = ByteArrayOutputStream();
    var customstd: PrintStream = PrintStream(customstdStream);
    System.setOut(customstd);
    System.setErr(customstd);

    var output: RuntimeOutput?

    try {
        var codeWrapper: Wrapper = Wrapper();
        output = codeWrapper.main(context)
    } catch (e: Exception) {
        var sw: StringWriter = StringWriter();
        var pw: PrintWriter = PrintWriter(sw);
        e.printStackTrace(pw);

        context.error(sw.toString());
        output = context.res.send("", 500, mutableMapOf<String, String>());
    }

    if(output == null) {
        context.error("Return statement missing. return context.res.empty() if no response is expected.");
        output = context.res.send("", 500, mutableMapOf<String, String>());
    }

    for (entry in output.headers.entries.iterator()) {
        var header: String = entry.key.lowercase()
        if(!(header.startsWith("x-open-runtimes-"))) {
            ctx.header(header, entry.value)
        }
    }

    if(!customstdStream.toString().isEmpty()) {
        context.log("Unsupported log noticed. Use context.log() or context.error() for logging.");
    }

    try {
        ctx.header("x-open-runtimes-logs", URLEncoder.encode(context.logs.joinToString("\n"), StandardCharsets.UTF_8.toString()));
        ctx.header("x-open-runtimes-errors", URLEncoder.encode(context.errors.joinToString("\n"), StandardCharsets.UTF_8.toString()));
    } catch (ex: UnsupportedEncodingException) {
        ctx.header("x-open-runtimes-logs", "Internal error while processing logs.");
        ctx.header("x-open-runtimes-errors", "Internal error while processing logs.");
    }

    ctx.status(output.statusCode).result(output.body)
    return
}