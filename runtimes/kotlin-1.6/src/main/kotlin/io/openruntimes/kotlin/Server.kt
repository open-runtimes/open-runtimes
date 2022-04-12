package io.openruntimes.kotlin

import io.javalin.Javalin
import io.javalin.http.Context
import io.javalin.plugin.json.jsonMapper
import io.openruntimes.kotlin.RuntimeResponse.Companion.error
import io.openruntimes.kotlin.RuntimeResponse.Companion.unauthorized
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
        val output = listOf(
            e.message ?? "", 
            e.stackTraceToString() ?? ""
        ).joinToString("\r\n")

        ctx.status(500).result(output)
    }
}