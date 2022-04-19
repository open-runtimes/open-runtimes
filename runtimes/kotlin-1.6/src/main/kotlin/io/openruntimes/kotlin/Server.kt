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

    try {
        ctx.result(codeWrapper.main(request, response).data)
    } catch (e: Exception) {
        ctx.status(500).result(e.stackTraceToString())
    }
}