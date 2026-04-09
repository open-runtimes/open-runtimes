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
import io.netty.handler.codec.http.QueryStringDecoder
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.withTimeoutOrNull
import java.io.PrintWriter
import java.io.StringWriter
import kotlin.reflect.full.callSuspend
import kotlin.reflect.full.createInstance
import kotlin.reflect.full.memberFunctions
import kotlin.time.Duration.Companion.seconds

val gson: Gson = GsonBuilder().serializeNulls().create()
val gsonInternal: Gson = GsonBuilder()
    .serializeNulls()
    .setObjectToNumberStrategy(ToNumberPolicy.LONG_OR_DOUBLE)
    .create()

suspend fun main() {
    val boss = NioEventLoopGroup(1)
    val worker = NioEventLoopGroup()

    try {
        val bootstrap = ServerBootstrap()
            .group(boss, worker)
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
        worker.shutdownGracefully()
    }
}

fun sendResponse(
    context: ChannelHandlerContext,
    status: Int,
    body: ByteArray,
    headers: Map<String, String> = emptyMap(),
) {
    val response = DefaultFullHttpResponse(
        HttpVersion.HTTP_1_1,
        HttpResponseStatus.valueOf(status),
        Unpooled.wrappedBuffer(body),
    )
    response.headers().set(HttpHeaderNames.CONTENT_LENGTH, body.size)
    for ((key, value) in headers) {
        response.headers().set(key, value)
    }
    context.writeAndFlush(response).addListener(ChannelFutureListener.CLOSE)
}

class RequestHandler : SimpleChannelInboundHandler<FullHttpRequest>() {
    private val scope = CoroutineScope(Dispatchers.Default)

    override fun channelRead0(context: ChannelHandlerContext, request: FullHttpRequest) {
        val decoder = QueryStringDecoder(request.uri())
        val path = decoder.path()

        if (path == "/__opr/health") {
            sendResponse(context, 200, "OK".toByteArray())
            return
        }

        if (path == "/__opr/timings") {
            val timings = java.io.File("/mnt/telemetry/timings.txt").readText()
            sendResponse(
                context,
                200,
                timings.toByteArray(),
                mapOf("content-type" to "text/plain; charset=utf-8"),
            )
            return
        }

        val method = request.method().name()
        val queryString = request.uri().let { uri ->
            val index = uri.indexOf('?')
            if (index >= 0) uri.substring(index + 1) else ""
        }
        val headers = mutableMapOf<String, String>()
        for (entry in request.headers()) {
            headers[entry.key.lowercase()] = entry.value
        }
        val bodyBytes = ByteArray(request.content().readableBytes())
        request.content().readBytes(bodyBytes)

        scope.launch {
            execute(context, method, path, queryString, headers, bodyBytes)
        }
    }

    override fun exceptionCaught(context: ChannelHandlerContext, cause: Throwable) {
        sendResponse(context, 500, "".toByteArray())
    }
}

suspend fun execute(
    context: ChannelHandlerContext,
    method: String,
    path: String,
    queryString: String,
    requestHeaders: MutableMap<String, String>,
    bodyBytes: ByteArray,
) {
    val logger = RuntimeLogger(
        requestHeaders["x-open-runtimes-logging"],
        requestHeaders["x-open-runtimes-log-id"],
    )
    try {
        action(logger, context, method, path, queryString, requestHeaders, bodyBytes)
    } catch (e: Exception) {
        val writer = StringWriter()
        e.printStackTrace(PrintWriter(writer))

        logger.write(arrayOf(writer.toString()), RuntimeLogger.TYPE_ERROR, false)
        logger.end()

        sendResponse(
            context,
            500,
            "".toByteArray(),
            mapOf("x-open-runtimes-log-id" to (logger.id ?: "")),
        )
    }
}

suspend fun action(
    logger: RuntimeLogger,
    context: ChannelHandlerContext,
    method: String,
    path: String,
    queryString: String,
    requestHeaders: MutableMap<String, String>,
    bodyBytes: ByteArray,
) {
    var safeTimeout = -1
    val timeout = requestHeaders["x-open-runtimes-timeout"] ?: ""
    if (timeout.isNotEmpty()) {
        var invalid = false
        try {
            safeTimeout = timeout.toInt()
        } catch (e: NumberFormatException) {
            invalid = true
        }

        if (invalid || safeTimeout < 0) {
            sendResponse(context, 500, "Header \"x-open-runtimes-timeout\" must be an integer greater than 0.".toByteArray())
            return
        }
    }

    val secret = requestHeaders["x-open-runtimes-secret"] ?: ""
    val serverSecret = System.getenv("OPEN_RUNTIMES_SECRET") ?: ""

    if (serverSecret != "" && secret != serverSecret) {
        sendResponse(context, 500, "Unauthorized. Provide correct \"x-open-runtimes-secret\" header.".toByteArray())
        return
    }

    val headers = mutableMapOf<String, String>()
    for ((key, value) in requestHeaders) {
        if (!key.startsWith("x-open-runtimes-")) {
            headers[key] = value
        }
    }

    var enforcedHeadersString = System.getenv("OPEN_RUNTIMES_HEADERS")
    if (enforcedHeadersString.isNullOrEmpty()) {
        enforcedHeadersString = "{}"
    }
    val enforcedHeaders = gsonInternal.fromJson(enforcedHeadersString, MutableMap::class.java)

    for (entry in enforcedHeaders.entries) {
        headers["${entry.key}".lowercase()] = "${entry.value}"
    }

    val hostHeader = requestHeaders["host"] ?: ""
    val protoHeader = requestHeaders["x-forwarded-proto"] ?: "http"

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
        bodyBytes,
        url,
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
            val writer = StringWriter()
            e.printStackTrace(PrintWriter(writer))

            runtimeContext.error(writer.toString())
            output = runtimeContext.res.text("", 500, mutableMapOf())
        } finally {
            logger.revertNativeLogs()
        }
    }

    if (output == null) {
        runtimeContext.error("Return statement missing. return context.res.empty() if no response is expected.")
        output = runtimeContext.res.text("", 500, mutableMapOf())
    }

    val responseHeaders = output.headers.mapKeys { it.key.lowercase() }.toMutableMap()

    if (responseHeaders["content-type"] == null) {
        responseHeaders["content-type"] = "text/plain"
    }

    if (!responseHeaders["content-type"]!!.startsWith("multipart/")) {
        responseHeaders["content-type"] = responseHeaders["content-type"]!!.lowercase()

        if (!responseHeaders["content-type"]!!.contains("charset=")) {
            responseHeaders["content-type"] += "; charset=utf-8"
        }
    }

    val finalHeaders = mutableMapOf<String, String>()
    responseHeaders.forEach { (key, value) ->
        if (!key.startsWith("x-open-runtimes-")) {
            finalHeaders[key] = value
        }
    }
    finalHeaders["x-open-runtimes-log-id"] = logger.id ?: ""

    logger.end()

    sendResponse(context, output.statusCode, output.body, finalHeaders)
}
