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

    if (secret.equals("") || secret != serverSecret) {
        ctx.status(500).result("Unauthorized. Provide correct \"x-open-runtimes-secret\" header.")
        return
    }

    var bodyString: String = ctx.body()
    var body: Any = bodyString
    var headers: MutableMap<String, String> = mutableMapOf<String, String>()
    var method: String = ctx.method()

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

    var hostHeader = ctx.header("host");
    var protoHeader = ctx.header("x-forwarded-proto");

    if(hostHeader == null) {
        hostHeader = "";
    }

    if(protoHeader == null) {
        protoHeader = "http";
    }

    var scheme: String = protoHeader
    var defaultPort: String = if (scheme.equals("https")) "443" else "80"

    var host: String
    var port: Int

    if(hostHeader.contains(":")) {
        host = hostHeader.split(":")[0]
        port = hostHeader.split(":")[1].toInt()
    } else {
        host = hostHeader
        port = Integer.parseInt(defaultPort)
    }

    var path: String = ctx.path()
    var queryString: String = ctx.queryString() ?: ""
    var query: MutableMap<String, String> = HashMap<String, String>()

    for(param in queryString.split("&")) {
        var pair = param.split("=", limit = 2)

        if(pair.size >= 1 && !pair[0].isEmpty()) {
            var value: String = if (pair.size == 2) pair[1] else ""
            query.put(pair[0], value)
        }
    }

    var url: String = scheme + "://" + host;

    if(port != Integer.parseInt(defaultPort)) {
        url += ":" + port;
    }

    url += path;

    if(!queryString.isEmpty()) {
        url += "?" + queryString;
    }

    var runtimeRequest: RuntimeRequest = RuntimeRequest(url, method, scheme, host, port, path, query, queryString, headers, body, bodyString)
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