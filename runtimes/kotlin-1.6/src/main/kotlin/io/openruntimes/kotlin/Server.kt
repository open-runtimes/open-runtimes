package io.openruntimes.kotlin

import io.javalin.Javalin
import io.javalin.http.Context
import io.javalin.plugin.json.jsonMapper
import kotlinx.coroutines.runBlocking

suspend fun main() {
    Javalin
        .create()
        .start(3000)
        .post("/") { runBlocking { execute(it) } }
}

suspend fun execute(ctx: Context) {
    RuntimeResponse.mapper = ctx.jsonMapper()

    if (ctx.header("x-internal-challenge").isNullOrBlank()) {
        ctx.status(500).result("Unauthorized");
        return;
    }
    if (ctx.header("x-internal-challenge") != System.getenv("INTERNAL_RUNTIME_KEY")) {
        ctx.status(500).result("Unauthorized")
        return
    }

    val codeWrapper = Wrapper()
    val request = RuntimeRequest(ctx)
    val response = RuntimeResponse()

    val outStream = ByteArrayOutputStream()
    val errStream = ByteArrayOutputStream()
    val userOut = PrintStream(outStream)
    val userErr = PrintStream(errStream)
    val systemOut = System.out
    val systemErr = System.err

    System.setOut(userOut)
    System.setErr(userErr)

    try {
        val userResponse = codeWrapper.main(request, response)
        val output = mutableMapOf(
            "response" to userResponse.data,
            "stdout" to outStream.toString()
        )
        ctx.result(output)
    } catch (e: Exception) {
        e.printStackTrace()
        val output = mutableMapOf(
            "stdout" to outStream.toString(),
            "stderr" to errStream.toString()
        )
        ctx.status(500).result(output)
    } finally {
         System.out.flush();
         System.err.flush();
         System.setOut(systemOut);
         System.setErr(systemErr);
    }
}