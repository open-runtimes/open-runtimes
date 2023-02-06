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
    var secret = ctx.header("x-open-runtimes-secret");
    var serverSecret = System.getenv("OPEN_RUNTIMES_SECRET");

    if(secret == null) {
        secret = "";
    }

    if(serverSecret == null) {
        serverSecret = "";
    }

    if (secret != serverSecret) {
        ctx.status(500).result("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.")
        return
    }

    var bodyString: String = ctx.body()
    var body: Any = bodyString
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
        if(!bodyString.isEmpty()) {
            var gson = GsonBuilder().serializeNulls().create();
            body = gson.fromJson(bodyString, MutableMap::class.java)
        } else {
            body = HashMap<String, Any>();
        }
    }

    var runtimeRequest: RuntimeRequest = RuntimeRequest(bodyString, body, headers, method, url)
    var runtimeResponse: RuntimeResponse = RuntimeResponse()
    var context: RuntimeContext = RuntimeContext(runtimeRequest, runtimeResponse)

    var systemOut = System.out;
    var systemErr = System.err;

    var customstdStream: ByteArrayOutputStream = ByteArrayOutputStream();
    var customstd: PrintStream = PrintStream(customstdStream);
    System.setOut(customstd);
    System.setErr(customstd);

    var output: RuntimeOutput?

    try {
        var classToLoad = Class.forName("io.openruntimes.kotlin.Tests");
        var classMethod = classToLoad.getDeclaredMethod("main", RuntimeContext::class.java);
        var instance = classToLoad.newInstance();
        output = classMethod.invoke(instance, context) as RuntimeOutput;
    } catch (e: Exception) {
        var sw: StringWriter = StringWriter();
        var pw: PrintWriter = PrintWriter(sw);
        e.printStackTrace(pw);

        context.error(sw.toString());
        output = context.res.send("", 500, mutableMapOf<String, String>());
    } finally {
        System.out.flush();
        System.err.flush();
        System.setOut(systemOut);
        System.setErr(systemErr);
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