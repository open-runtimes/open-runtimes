package io.openruntimes.kotlin

import com.google.gson.Gson
import com.google.gson.GsonBuilder
import com.google.gson.ToNumberPolicy
import io.netty.bootstrap.ServerBootstrap
import io.netty.buffer.Unpooled
import io.netty.channel.ChannelFutureListener
import io.netty.channel.ChannelHandlerContext
import io.netty.channel.ChannelInitializer
import io.netty.channel.ChannelOption
import io.netty.channel.SimpleChannelInboundHandler
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.codec.http.DefaultFullHttpResponse
import io.netty.handler.codec.http.FullHttpRequest
import io.netty.handler.codec.http.HttpHeaderNames
import io.netty.handler.codec.http.HttpHeaderValues
import io.netty.handler.codec.http.HttpObjectAggregator
import io.netty.handler.codec.http.HttpResponseStatus
import io.netty.handler.codec.http.HttpServerCodec
import io.netty.handler.codec.http.HttpUtil
import io.netty.handler.codec.http.HttpVersion
import kotlinx.coroutines.CoroutineScope
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.launch
import kotlinx.coroutines.withTimeoutOrNull
import java.io.PrintWriter
import java.io.StringWriter
import java.nio.charset.StandardCharsets
import kotlin.reflect.KFunction
import kotlin.reflect.full.callSuspend
import kotlin.reflect.full.createInstance
import kotlin.reflect.full.memberFunctions
import kotlin.time.Duration.Companion.seconds

val gson: Gson = GsonBuilder().serializeNulls().create()
val gsonInternal: Gson = GsonBuilder().serializeNulls().setObjectToNumberStrategy(ToNumberPolicy.LONG_OR_DOUBLE).create()

private val serverSecret: String = System.getenv("OPEN_RUNTIMES_SECRET") ?: ""
private val enforcedHeaders: Map<String, Any> = run {
    val json = System.getenv("OPEN_RUNTIMES_HEADERS")
    if (json.isNullOrEmpty()) emptyMap()
    else gsonInternal.fromJson(json, MutableMap::class.java) as Map<String, Any>
}

private var cachedMethod: KFunction<*>? = null
private var cachedInstance: Any? = null
private var classLoadError: String? = null

private fun loadUserClass() {
    val entrypoint = System.getenv("OPEN_RUNTIMES_ENTRYPOINT")
    try {
        val className = entrypoint.substring(0, entrypoint.length - 3).replace('/', '.')
        val classToLoad = Class.forName("io.openruntimes.kotlin.$className").kotlin
        cachedMethod = classToLoad.memberFunctions.find { it.name == "main" }
        if (cachedMethod == null) {
            throw NoSuchMethodException("Function signature invalid. Did you forget to export a 'main' function?")
        }
        cachedInstance = classToLoad.createInstance()
    } catch (e: ClassNotFoundException) {
        classLoadError = "Class not found: ${e.message}"
    } catch (e: NoSuchMethodException) {
        classLoadError = e.message ?: "Function signature invalid."
    } catch (e: Exception) {
        classLoadError = "Failed to load module: ${e.message}"
    }
}

suspend fun main() {
    loadUserClass()

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
            .option(ChannelOption.SO_BACKLOG, 1024)
            .childOption(ChannelOption.SO_KEEPALIVE, true)

        val future = bootstrap.bind(3000).sync()
        println("HTTP server successfully started!")
        future.channel().closeFuture().sync()
    } finally {
        boss.shutdownGracefully()
        workers.shutdownGracefully()
    }
}

private fun sendResponse(
    ctx: ChannelHandlerContext,
    keepAlive: Boolean,
    status: HttpResponseStatus,
    body: ByteArray,
    headers: Map<String, String> = emptyMap()
) {
    val response = DefaultFullHttpResponse(HttpVersion.HTTP_1_1, status, Unpooled.wrappedBuffer(body))
    response.headers().setInt(HttpHeaderNames.CONTENT_LENGTH, body.size)
    for ((key, value) in headers) {
        response.headers().set(key, value)
    }
    if (keepAlive) {
        response.headers().set(HttpHeaderNames.CONNECTION, HttpHeaderValues.KEEP_ALIVE)
        ctx.writeAndFlush(response)
    } else {
        ctx.writeAndFlush(response).addListener(ChannelFutureListener.CLOSE)
    }
}

class RequestHandler : SimpleChannelInboundHandler<FullHttpRequest>() {
    private val scope = CoroutineScope(Dispatchers.Default)

    override fun channelRead0(ctx: ChannelHandlerContext, request: FullHttpRequest) {
        val uri = request.uri()
        val questionMark = uri.indexOf('?')
        val path = if (questionMark >= 0) uri.substring(0, questionMark) else uri
        val keepAlive = HttpUtil.isKeepAlive(request)

        if (path == "/__opr/health") {
            sendResponse(ctx, keepAlive, HttpResponseStatus.OK, "OK".toByteArray(StandardCharsets.UTF_8))
            return
        }

        if (path == "/__opr/timings") {
            val timings = java.io.File("/mnt/telemetry/timings.txt").readText()
            sendResponse(
                ctx, keepAlive, HttpResponseStatus.OK,
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

        val queryString = if (questionMark >= 0) uri.substring(questionMark + 1) else ""

        scope.launch {
            try {
                execute(ctx, keepAlive, method, path, queryString, bodyBinary, requestHeaders)
            } catch (e: Exception) {
                val sw = StringWriter()
                val pw = PrintWriter(sw)
                e.printStackTrace(pw)
                sendResponse(ctx, keepAlive, HttpResponseStatus.INTERNAL_SERVER_ERROR, ByteArray(0))
            }
        }
    }

    override fun exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) {
        ctx.close()
    }
}

suspend fun execute(
    ctx: ChannelHandlerContext,
    keepAlive: Boolean,
    method: String,
    path: String,
    queryString: String,
    bodyBinary: ByteArray,
    requestHeaders: MutableMap<String, String>
) {
    val logger = RuntimeLogger(
        requestHeaders["x-open-runtimes-logging"],
        requestHeaders["x-open-runtimes-log-id"]
    )

    try {
        var safeTimeout = -1
        val timeoutHeader = requestHeaders["x-open-runtimes-timeout"] ?: ""
        if (timeoutHeader.isNotEmpty()) {
            var invalid = false
            try {
                safeTimeout = timeoutHeader.toInt()
            } catch (e: NumberFormatException) {
                invalid = true
            }

            if (invalid || safeTimeout < 0) {
                sendResponse(ctx, keepAlive, HttpResponseStatus.INTERNAL_SERVER_ERROR, "Header \"x-open-runtimes-timeout\" must be an integer greater than 0.".toByteArray(StandardCharsets.UTF_8))
                return
            }
        }

        val secretHeader = requestHeaders["x-open-runtimes-secret"] ?: ""

        if (serverSecret != "" && secretHeader != serverSecret) {
            sendResponse(ctx, keepAlive, HttpResponseStatus.INTERNAL_SERVER_ERROR, "Unauthorized. Provide correct \"x-open-runtimes-secret\" header.".toByteArray(StandardCharsets.UTF_8))
            return
        }

        val headers = mutableMapOf<String, String>()
        for ((key, value) in requestHeaders) {
            if (!key.startsWith("x-open-runtimes-")) {
                headers[key] = value
            }
        }

        for ((key, value) in enforcedHeaders) {
            headers[key.lowercase()] = "$value"
        }

        val protoHeader = requestHeaders["x-forwarded-proto"] ?: "http"
        val defaultPort = if (protoHeader == "https") "443" else "80"
        val hostHeader = requestHeaders["host"] ?: ""

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

        val runtimeRequest = RuntimeRequest(method, protoHeader, host, port, path, query, queryString, headers, bodyBinary, url)
        val runtimeResponse = RuntimeResponse()
        val runtimeContext = RuntimeContext(runtimeRequest, runtimeResponse, logger)

        logger.overrideNativeLogs()

        var output: RuntimeOutput? = null

        if (classLoadError != null) {
            runtimeContext.error(classLoadError!!)
            logger.revertNativeLogs()
            output = runtimeContext.res.text("", 503, mutableMapOf())
        }

        if (output == null && cachedMethod != null && cachedInstance != null) {
            try {
                if (safeTimeout > 0) {
                    output = withTimeoutOrNull(safeTimeout.seconds) {
                        if (cachedMethod!!.isSuspend) {
                            cachedMethod!!.callSuspend(cachedInstance, runtimeContext) as RuntimeOutput
                        } else {
                            cachedMethod!!.call(cachedInstance, runtimeContext) as RuntimeOutput
                        }
                    }

                    if (output == null) {
                        runtimeContext.error("Execution timed out.")
                        output = runtimeContext.res.text("", 500)
                    }
                } else {
                    output = if (cachedMethod!!.isSuspend) {
                        cachedMethod!!.callSuspend(cachedInstance, runtimeContext) as RuntimeOutput
                    } else {
                        cachedMethod!!.call(cachedInstance, runtimeContext) as RuntimeOutput
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

        sendResponse(ctx, keepAlive, HttpResponseStatus.valueOf(output.statusCode), output.body, responseHeaders)

    } catch (e: Exception) {
        val sw = StringWriter()
        val pw = PrintWriter(sw)
        e.printStackTrace(pw)

        try {
            logger.write(arrayOf(sw.toString()), RuntimeLogger.TYPE_ERROR, false)
            logger.end()
        } catch (_: IOException) {}

        sendResponse(
            ctx, keepAlive, HttpResponseStatus.INTERNAL_SERVER_ERROR, ByteArray(0),
            mapOf("x-open-runtimes-log-id" to (logger.id ?: ""))
        )
    }
}
