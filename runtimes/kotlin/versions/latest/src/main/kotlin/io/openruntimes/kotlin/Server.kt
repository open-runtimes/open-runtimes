package io.openruntimes.kotlin

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import com.google.gson.ToNumberPolicy
import io.netty.bootstrap.ServerBootstrap
import io.netty.buffer.Unpooled
import io.netty.channel.ChannelFutureListener
import io.netty.channel.ChannelHandlerContext
import io.netty.channel.ChannelInitializer
import io.netty.channel.SimpleChannelInboundHandler
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.codec.http.DefaultFullHttpResponse
import io.netty.handler.codec.http.FullHttpRequest
import io.netty.handler.codec.http.HttpHeaderNames
import io.netty.handler.codec.http.HttpObjectAggregator
import io.netty.handler.codec.http.HttpResponseStatus
import io.netty.handler.codec.http.HttpServerCodec
import io.netty.handler.codec.http.HttpVersion
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.withTimeoutOrNull
import java.io.PrintWriter
import java.io.StringWriter
import java.nio.charset.StandardCharsets
import kotlin.reflect.full.callSuspend
import kotlin.reflect.full.createInstance
import kotlin.reflect.full.memberFunctions
import kotlin.time.Duration.Companion.seconds

val gson: Gson = GsonBuilder().serializeNulls().create()
val gsonInternal: Gson = GsonBuilder().serializeNulls().setObjectToNumberStrategy(ToNumberPolicy.LONG_OR_DOUBLE).create()

suspend fun main() {
    val boss = NioEventLoopGroup(1)
    val workers = NioEventLoopGroup()

    try {
        val bootstrap = ServerBootstrap()
            .group(boss, workers)
            .channel(NioServerSocketChannel::class.java)
            .childHandler(object : ChannelInitializer<SocketChannel>() {
                override fun initChannel(channel: SocketChannel) {
                    channel.pipeline()
                        .addLast(HttpServerCodec())
                        .addLast(HttpObjectAggregator(20 * 1024 * 1024))
                        .addLast(RequestHandler())
                }
            })

        val future = bootstrap.bind(3000).sync()
        println("HTTP server successfully started!")
        future.channel().closeFuture().sync()
    } finally {
        boss.shutdownGracefully()
        workers.shutdownGracefully()
    }
}

private fun sendResponse(context: ChannelHandlerContext, status: HttpResponseStatus, body: ByteArray, headers: Map<String, String> = emptyMap()) {
    val response = DefaultFullHttpResponse(HttpVersion.HTTP_1_1, status, Unpooled.wrappedBuffer(body))
    response.headers().set(HttpHeaderNames.CONTENT_LENGTH, body.size)
    for ((key, value) in headers) {
        response.headers().set(key, value)
    }
    context.writeAndFlush(response).addListener(ChannelFutureListener.CLOSE)
}

class RequestHandler : SimpleChannelInboundHandler<FullHttpRequest>() {
    private val scope = CoroutineScope(Dispatchers.Default)

    override fun channelRead0(context: ChannelHandlerContext, request: FullHttpRequest) {
        val uri = request.uri()
        val path = if (uri.contains("?")) uri.substringBefore("?") else uri

        if (path == "/__opr/health") {
            sendResponse(context, HttpResponseStatus.OK, "OK".toByteArray(StandardCharsets.UTF_8))
            return
        }

        if (path == "/__opr/timings") {
            val timings = java.io.File("/mnt/telemetry/timings.txt").readText()
            sendResponse(
                context,
                HttpResponseStatus.OK,
                timings.toByteArray(StandardCharsets.UTF_8),
                mapOf("content-type" to "text/plain; charset=utf-8")
            )
            return
        }

        val method = request.method().name()
        val bodyBinary = ByteArray(request.content().readableBytes())
        request.content().readBytes(bodyBinary)

        val requestHeaders = mutableMapOf<String, String>()
        for (entry in request.headers()) {
            requestHeaders[entry.key.lowercase()] = entry.value
        }

        val loggingHeader = requestHeaders["x-open-runtimes-logging"]
        val logIdHeader = requestHeaders["x-open-runtimes-log-id"]
        val timeoutHeader = requestHeaders["x-open-runtimes-timeout"] ?: ""
        val secretHeader = requestHeaders["x-open-runtimes-secret"] ?: ""
        val hostHeader = requestHeaders["host"] ?: ""
        val protoHeader = requestHeaders["x-forwarded-proto"] ?: "http"

        val queryString = if (uri.contains("?")) uri.substringAfter("?") else ""

        scope.launch {
            val logger = RuntimeLogger(loggingHeader, logIdHeader)
            try {
                execute(context, logger, method, path, queryString, bodyBinary, requestHeaders, timeoutHeader, secretHeader, hostHeader, protoHeader)
            } catch (e: Exception) {
                val sw = StringWriter()
                val pw = PrintWriter(sw)
                e.printStackTrace(pw)

                val message = sw.toString()

                logger.write(arrayOf(message), RuntimeLogger.TYPE_ERROR, false)
                logger.end()

                val headers = mapOf("x-open-runtimes-log-id" to (logger.id ?: ""))
                sendResponse(context, HttpResponseStatus.INTERNAL_SERVER_ERROR, ByteArray(0), headers)
            }
        }
    }

    @Suppress("DEPRECATION")
    override fun exceptionCaught(context: ChannelHandlerContext, cause: Throwable) {
        cause.printStackTrace()
        context.close()
    }
}

suspend fun execute(
    context: ChannelHandlerContext,
    logger: RuntimeLogger,
    method: String,
    path: String,
    queryString: String,
    bodyBinary: ByteArray,
    requestHeaders: MutableMap<String, String>,
    timeoutHeader: String,
    secretHeader: String,
    hostHeader: String,
    protoHeader: String
) {
    var safeTimeout = -1
    if (timeoutHeader.isNotEmpty()) {
        var invalid = false
        try {
            safeTimeout = timeoutHeader.toInt()
        } catch (e: NumberFormatException) {
            invalid = true
        }

        if (invalid || safeTimeout < 0) {
            sendResponse(context, HttpResponseStatus.INTERNAL_SERVER_ERROR, "Header \"x-open-runtimes-timeout\" must be an integer greater than 0.".toByteArray(StandardCharsets.UTF_8))
            return
        }
    }

    val serverSecret = System.getenv("OPEN_RUNTIMES_SECRET") ?: ""

    if (serverSecret != "" && secretHeader != serverSecret) {
        sendResponse(context, HttpResponseStatus.INTERNAL_SERVER_ERROR, "Unauthorized. Provide correct \"x-open-runtimes-secret\" header.".toByteArray(StandardCharsets.UTF_8))
        return
    }

    val headers = mutableMapOf<String, String>()
    for ((key, value) in requestHeaders) {
        if (!key.startsWith("x-open-runtimes-")) {
            headers[key] = value
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

    if (queryString.isNotEmpty()) {
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
        bodyBinary,
        url
    )
    val runtimeResponse = RuntimeResponse()
    val runtimeContext = RuntimeContext(runtimeRequest, runtimeResponse, logger)

    logger.overrideNativeLogs()

    var output: RuntimeOutput? = null
    var classMethod: kotlin.reflect.KFunction<*>? = null
    var instance: Any? = null

    val entrypoint = System.getenv("OPEN_RUNTIMES_ENTRYPOINT")

    try {
        val className = entrypoint
            .substring(0, entrypoint.length - 3)
            .replace('/', '.')

        val classToLoad = Class.forName("io.openruntimes.kotlin.$className").kotlin
        classMethod = classToLoad.memberFunctions.find { it.name == "main" }
        if (classMethod == null) {
            throw NoSuchMethodException("Function signature invalid. Did you forget to export a 'main' function?")
        }
        instance = classToLoad.createInstance()
    } catch (e: ClassNotFoundException) {
        runtimeContext.error("Class not found: ${e.message}")
        logger.revertNativeLogs()
        output = runtimeContext.res.text("", 503, mutableMapOf())
    } catch (e: NoSuchMethodException) {
        runtimeContext.error(e.message ?: "Function signature invalid. Did you forget to export a 'main' function?")
        logger.revertNativeLogs()
        output = runtimeContext.res.text("", 503, mutableMapOf())
    } catch (e: InstantiationException) {
        runtimeContext.error("Failed to create instance: ${e.message}")
        logger.revertNativeLogs()
        output = runtimeContext.res.text("", 503, mutableMapOf())
    } catch (e: IllegalAccessException) {
        runtimeContext.error("Access denied: ${e.message}")
        logger.revertNativeLogs()
        output = runtimeContext.res.text("", 503, mutableMapOf())
    } catch (e: Exception) {
        runtimeContext.error("Failed to load module: ${e.message}")
        logger.revertNativeLogs()
        output = runtimeContext.res.text("", 503, mutableMapOf())
    }

    if (output == null && classMethod != null && instance != null) {
        try {
            if (safeTimeout > 0) {
                output = withTimeoutOrNull(safeTimeout.seconds) {
                    if (classMethod.isSuspend) {
                        classMethod.callSuspend(instance, runtimeContext) as RuntimeOutput
                    } else {
                        classMethod.call(instance, runtimeContext) as RuntimeOutput
                    }
                }

                if (output == null) {
                    runtimeContext.error("Execution timed out.")
                    output = runtimeContext.res.text("", 500)
                }
            } else {
                output = if (classMethod.isSuspend) {
                    classMethod.callSuspend(instance, runtimeContext) as RuntimeOutput
                } else {
                    classMethod.call(instance, runtimeContext) as RuntimeOutput
                }
            }
        } catch (e: Exception) {
            val sw = StringWriter()
            val pw = PrintWriter(sw)
            e.printStackTrace(pw)

            runtimeContext.error(sw.toString())
            output = runtimeContext.res.text("", 500, mutableMapOf())
        } finally {
            logger.revertNativeLogs()
        }
    }

    if (output == null) {
        runtimeContext.error("Return statement missing. return context.res.empty() if no response is expected.")
        output = runtimeContext.res.text("", 500, mutableMapOf())
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

    val responseHeaders = mutableMapOf<String, String>()
    resHeaders.forEach { (key, value) ->
        if (!key.startsWith("x-open-runtimes-")) {
            responseHeaders[key] = value
        }
    }

    responseHeaders["x-open-runtimes-log-id"] = logger.id ?: ""

    logger.end()

    sendResponse(context, HttpResponseStatus.valueOf(output.statusCode), output.body, responseHeaders)
}
