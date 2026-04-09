package io.openruntimes.java;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.ToNumberPolicy;
import io.netty.bootstrap.ServerBootstrap;
import io.netty.buffer.ByteBuf;
import io.netty.buffer.Unpooled;
import io.netty.channel.Channel;
import io.netty.channel.ChannelFutureListener;
import io.netty.channel.ChannelHandlerContext;
import io.netty.channel.ChannelInitializer;
import io.netty.channel.ChannelOption;
import io.netty.channel.EventLoopGroup;
import io.netty.channel.SimpleChannelInboundHandler;
import io.netty.channel.nio.NioEventLoopGroup;
import io.netty.channel.socket.SocketChannel;
import io.netty.channel.socket.nio.NioServerSocketChannel;
import io.netty.handler.codec.http.DefaultFullHttpResponse;
import io.netty.handler.codec.http.FullHttpRequest;
import io.netty.handler.codec.http.FullHttpResponse;
import io.netty.handler.codec.http.HttpHeaderNames;
import io.netty.handler.codec.http.HttpObjectAggregator;
import io.netty.handler.codec.http.HttpResponseStatus;
import io.netty.handler.codec.http.HttpServerCodec;
import io.netty.handler.codec.http.HttpVersion;
import io.netty.handler.codec.http.QueryStringDecoder;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

public class Server {

    private static final Gson gson = new GsonBuilder().serializeNulls().create();
    private static final Gson gsonInternal =
        new GsonBuilder()
            .serializeNulls()
            .setObjectToNumberStrategy(ToNumberPolicy.LONG_OR_DOUBLE)
            .create();

    private static final ExecutorService executor;

    static {
        ExecutorService temporary;
        try {
            temporary = (ExecutorService) Executors.class
                .getMethod("newVirtualThreadPerTaskExecutor")
                .invoke(null);
        } catch (Exception e) {
            temporary = Executors.newCachedThreadPool();
        }
        executor = temporary;
    }

    public static void main(String[] args) throws Exception {
        EventLoopGroup boss = new NioEventLoopGroup(1);
        EventLoopGroup workers = new NioEventLoopGroup();

        try {
            ServerBootstrap bootstrap = new ServerBootstrap();
            bootstrap.group(boss, workers)
                .channel(NioServerSocketChannel.class)
                .childHandler(new ChannelInitializer<SocketChannel>() {
                    @Override
                    protected void initChannel(SocketChannel channel) {
                        channel.pipeline()
                            .addLast(new HttpServerCodec())
                            .addLast(new HttpObjectAggregator(20 * 1024 * 1024))
                            .addLast(new RequestHandler());
                    }
                })
                .option(ChannelOption.SO_BACKLOG, 1024)
                .childOption(ChannelOption.SO_KEEPALIVE, true);

            Channel channel = bootstrap.bind(3000).sync().channel();
            System.out.println("HTTP server successfully started!");
            channel.closeFuture().sync();
        } finally {
            boss.shutdownGracefully();
            workers.shutdownGracefully();
        }
    }

    private static void sendResponse(ChannelHandlerContext context, int status, byte[] body, Map<String, String> headers) {
        FullHttpResponse response = new DefaultFullHttpResponse(
            HttpVersion.HTTP_1_1,
            HttpResponseStatus.valueOf(status),
            Unpooled.wrappedBuffer(body)
        );

        response.headers().set(HttpHeaderNames.CONTENT_LENGTH, body.length);
        for (Map.Entry<String, String> entry : headers.entrySet()) {
            response.headers().set(entry.getKey(), entry.getValue());
        }

        context.writeAndFlush(response).addListener(ChannelFutureListener.CLOSE);
    }

    private static class RequestHandler extends SimpleChannelInboundHandler<FullHttpRequest> {

        @Override
        protected void channelRead0(ChannelHandlerContext context, FullHttpRequest request) {
            QueryStringDecoder decoder = new QueryStringDecoder(request.uri());
            String path = decoder.path();

            if (path.equals("/__opr/health")) {
                sendResponse(context, 200, "OK".getBytes(StandardCharsets.UTF_8), new HashMap<>());
                return;
            }

            if (path.equals("/__opr/timings")) {
                try {
                    byte[] timings = Files.readAllBytes(Paths.get("/mnt/telemetry/timings.txt"));
                    Map<String, String> headers = new HashMap<>();
                    headers.put("content-type", "text/plain; charset=utf-8");
                    sendResponse(context, 200, timings, headers);
                } catch (IOException e) {
                    sendResponse(context, 500, "Error reading timings".getBytes(StandardCharsets.UTF_8), new HashMap<>());
                }
                return;
            }

            String method = request.method().name();
            String queryString = request.uri().contains("?")
                ? request.uri().substring(request.uri().indexOf('?') + 1)
                : "";

            Map<String, String> headers = new HashMap<>();
            for (Map.Entry<String, String> entry : request.headers()) {
                headers.put(entry.getKey().toLowerCase(), entry.getValue());
            }

            ByteBuf content = request.content();
            byte[] body = new byte[content.readableBytes()];
            content.readBytes(body);

            executor.submit(() -> {
                execute(context, method, path, queryString, headers, body);
            });
        }

        @Override
        public void exceptionCaught(ChannelHandlerContext context, Throwable cause) {
            sendResponse(context, 500, new byte[0], new HashMap<>());
        }
    }

    private static void execute(
        ChannelHandlerContext context,
        String method,
        String path,
        String queryString,
        Map<String, String> requestHeaders,
        byte[] bodyBytes
    ) {
        RuntimeLogger logger = null;

        try {
            logger = new RuntimeLogger(
                requestHeaders.get("x-open-runtimes-logging"),
                requestHeaders.get("x-open-runtimes-log-id")
            );
        } catch (IOException e) {
            try {
                logger = new RuntimeLogger("disabled", "");
            } catch (IOException ignored) {
            }
        }

        try {
            action(context, logger, method, path, queryString, requestHeaders, bodyBytes);
        } catch (Exception e) {
            StringWriter writer = new StringWriter();
            PrintWriter printer = new PrintWriter(writer);
            e.printStackTrace(printer);
            String message = writer.toString();

            Map<String, String> responseHeaders = new HashMap<>();
            responseHeaders.put("x-open-runtimes-log-id", logger.getId());

            try {
                String[] logs = new String[1];
                logs[0] = message;
                logger.write(logs, RuntimeLogger.TYPE_ERROR, false);
                logger.end();
            } catch (IOException ignored) {
            }

            sendResponse(context, 500, new byte[0], responseHeaders);
        }
    }

    private static void action(
        ChannelHandlerContext context,
        RuntimeLogger logger,
        String method,
        String path,
        String queryString,
        Map<String, String> requestHeaders,
        byte[] bodyBytes
    ) {
        int safeTimeout = -1;
        String timeout = requestHeaders.get("x-open-runtimes-timeout");
        if (timeout != null && !timeout.isEmpty()) {
            boolean invalid = false;

            try {
                safeTimeout = Integer.parseInt(timeout);
            } catch (NumberFormatException e) {
                invalid = true;
            }

            if (invalid || safeTimeout == 0) {
                sendResponse(
                    context, 500,
                    "Header \"x-open-runtimes-timeout\" must be an integer greater than 0.".getBytes(StandardCharsets.UTF_8),
                    new HashMap<>()
                );
                return;
            }
        }

        String serverSecret = System.getenv("OPEN_RUNTIMES_SECRET");
        if (serverSecret == null) {
            serverSecret = "";
        }

        String secret = requestHeaders.get("x-open-runtimes-secret");
        if (secret == null) {
            secret = "";
        }

        if (!serverSecret.equals("") && !secret.equals(serverSecret)) {
            sendResponse(
                context, 500,
                "Unauthorized. Provide correct \"x-open-runtimes-secret\" header.".getBytes(StandardCharsets.UTF_8),
                new HashMap<>()
            );
            return;
        }

        Map<String, String> headers = new HashMap<>();
        for (Map.Entry<String, String> entry : requestHeaders.entrySet()) {
            String header = entry.getKey().toLowerCase();
            if (!header.startsWith("x-open-runtimes-")) {
                headers.put(header, entry.getValue());
            }
        }

        String enforcedHeadersString = System.getenv("OPEN_RUNTIMES_HEADERS");
        if (enforcedHeadersString == null || enforcedHeadersString.isEmpty()) {
            enforcedHeadersString = "{}";
        }

        Map<String, Object> enforcedHeaders = gsonInternal.fromJson(enforcedHeadersString, Map.class);
        for (Map.Entry<String, Object> entry : enforcedHeaders.entrySet()) {
            headers.put(entry.getKey().toLowerCase(), String.valueOf(entry.getValue()));
        }

        String scheme = requestHeaders.get("x-forwarded-proto");
        if (scheme == null) {
            scheme = "http";
        }

        String defaultPort = scheme.equals("https") ? "443" : "80";

        String hostHeader = requestHeaders.get("host");
        if (hostHeader == null) {
            hostHeader = "";
        }

        String host;
        int port;

        if (hostHeader.contains(":")) {
            host = hostHeader.split(":")[0];
            port = Integer.parseInt(hostHeader.split(":")[1]);
        } else {
            host = hostHeader;
            port = Integer.parseInt(defaultPort);
        }

        Map<String, String> query = new HashMap<>();
        for (String param : queryString.split("&")) {
            String[] pair = param.split("=", 2);
            if (pair.length >= 1 && pair[0] != null && !pair[0].isEmpty()) {
                String value = pair.length == 2 ? pair[1] : "";
                query.put(pair[0], value);
            }
        }

        String url = scheme + "://" + host;
        if (port != Integer.parseInt(defaultPort)) {
            url += ":" + port;
        }
        url += path;
        if (!queryString.isEmpty()) {
            url += "?" + queryString;
        }

        RuntimeRequest runtimeRequest = new RuntimeRequest(
            method, scheme, host, port, path, query, queryString, headers, bodyBytes, url
        );
        RuntimeResponse runtimeResponse = new RuntimeResponse();
        RuntimeContext runtimeContext = new RuntimeContext(runtimeRequest, runtimeResponse, logger);

        logger.overrideNativeLogs();

        RuntimeOutput output = null;
        Method classMethod = null;
        Object instance = null;

        String entrypoint = System.getenv("OPEN_RUNTIMES_ENTRYPOINT");

        try {
            String className = entrypoint.substring(0, entrypoint.length() - 5);
            className = className.replaceAll("/", ".");

            final Class classToLoad = Class.forName("io.openruntimes.java." + className);
            classMethod = classToLoad.getDeclaredMethod("main", RuntimeContext.class);
            instance = classToLoad.newInstance();
        } catch (ClassNotFoundException e) {
            runtimeContext.error("Class not found: " + e.getMessage());
            logger.revertNativeLogs();
            output = runtimeContext.getRes().send("", 503);
        } catch (NoSuchMethodException e) {
            runtimeContext.error("Function signature invalid. Did you forget to export a 'main' function?");
            logger.revertNativeLogs();
            output = runtimeContext.getRes().send("", 503);
        } catch (InstantiationException e) {
            runtimeContext.error("Failed to create instance: " + e.getMessage());
            logger.revertNativeLogs();
            output = runtimeContext.getRes().send("", 503);
        } catch (IllegalAccessException e) {
            runtimeContext.error("Access denied: " + e.getMessage());
            logger.revertNativeLogs();
            output = runtimeContext.getRes().send("", 503);
        } catch (Exception e) {
            runtimeContext.error("Failed to load module: " + e.getMessage());
            logger.revertNativeLogs();
            output = runtimeContext.getRes().send("", 503);
        }

        if (output == null && classMethod != null && instance != null) {
            final Method finalClassMethod = classMethod;
            final Object finalInstance = instance;

            try {
                if (safeTimeout > 0) {
                    Future<RuntimeOutput> future = executor.submit(() -> {
                        try {
                            return (RuntimeOutput) finalClassMethod.invoke(finalInstance, runtimeContext);
                        } catch (Exception e) {
                            StringWriter writer = new StringWriter();
                            PrintWriter printer = new PrintWriter(writer);
                            e.printStackTrace(printer);

                            runtimeContext.error(writer.toString());
                            runtimeContext.getRes().send("", 500);
                        }
                        return null;
                    });

                    try {
                        output = future.get(safeTimeout, TimeUnit.SECONDS);
                    } catch (TimeoutException e) {
                        future.cancel(true);
                        runtimeContext.error("Execution timed out.");
                        output = runtimeContext.getRes().send("", 500);
                    }
                } else {
                    output = (RuntimeOutput) classMethod.invoke(instance, runtimeContext);
                }
            } catch (Exception e) {
                StringWriter writer = new StringWriter();
                PrintWriter printer = new PrintWriter(writer);
                e.printStackTrace(printer);

                runtimeContext.error(writer.toString());
                output = runtimeContext.getRes().send("", 500);
            } finally {
                logger.revertNativeLogs();
            }
        }

        if (output == null) {
            runtimeContext.error("Return statement missing. return context.res.empty() if no response is expected.");
            output = runtimeContext.getRes().send("", 500);
        }

        output.getHeaders().putIfAbsent("content-type", "text/plain");

        Map<String, String> responseHeaders = new HashMap<>();

        for (Map.Entry<String, String> entry : output.getHeaders().entrySet()) {
            String header = entry.getKey().toLowerCase();
            String headerValue = entry.getValue();

            if (header.startsWith("x-open-runtimes-")) {
                continue;
            }

            if (header.equals("content-type") && !headerValue.startsWith("multipart/")) {
                headerValue = headerValue.toLowerCase();
                if (!headerValue.contains("charset=")) {
                    headerValue += "; charset=utf-8";
                }
            }

            responseHeaders.put(header, headerValue);
        }

        responseHeaders.put("x-open-runtimes-log-id", logger.getId());

        try {
            logger.end();
        } catch (IOException ignored) {
        }

        sendResponse(context, output.getStatusCode(), output.getBody(), responseHeaders);
    }
}
