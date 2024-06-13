package io.openruntimes.kotlin

import com.google.gson.Gson
import kotlinx.coroutines.coroutineScope
import kotlinx.coroutines.delay
import kotlinx.coroutines.runBlocking
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.HttpURLConnection
import java.net.URL

public class Tests {
    @Throws(Exception::class)
    suspend fun main(context: RuntimeContext): RuntimeOutput {
        when (context.req.headers["x-action"]) {
            "plaintextResponse" -> {
                return context.res.text("Hello World 👋")
            }
            "jsonResponse" -> {
                return context.res.json(mutableMapOf(
                    "json" to true,
                    "message" to "Developers are awesome."
                ))
            }
            "customCharsetResponse" -> {
                return context.res.text("ÅÆ", 200, mutableMapOf(
                    "content-type" to "text/plain; charset=iso-8859-1"
                ))
            }
            "multipartResponse" -> {
                return context.res.text("""--12345
Content-Disposition: form-data; name=\"partOne\"

Why just have one part?
--12345
Content-Disposition: form-data; name=\"partTwo\"

When you can have two!
--12345--""", 200, mutableMapOf(
                    "content-type" to "multipart/form-data; boundary=12345"
                ))
            }
            "redirectResponse" -> {
                return context.res.redirect("https://github.com/")
            }
            "emptyResponse" -> {
                return context.res.empty()
            }
            "noResponse" -> {
                context.res.text("This should be ignored, as it is not returned.")

                // Simulate test data. Return nessessary in Java
                context.error("Return statement missing. return context.res.empty() if no response is expected.")
                return context.res.text("", 500)
            }
            "doubleResponse" -> {
                context.res.text("This should be ignored.")
                return context.res.text("This should be returned.")
            }
            "headersResponse" -> {
                return context.res.text("OK", 200, mutableMapOf(
                    "first-header" to "first-value",
                    "second-header" to context.req.headers.getOrDefault("x-open-runtimes-custom-in-header", "missing"),
                    "cookie" to context.req.headers.getOrDefault("cookie", "missing"),
                    "x-open-runtimes-custom-out-header" to "third-value"
                ))
            }
            "statusResponse" -> {
                return context.res.text("FAIL", 404)
            }
            "requestMethod" -> {
                return context.res.text(context.req.method)
            }
            "requestUrl" -> {
                return context.res.json(mutableMapOf(
                    "url" to context.req.url,
                    "port" to context.req.port,
                    "path" to context.req.path,
                    "query" to context.req.query,
                    "queryString" to context.req.queryString,
                    "scheme" to context.req.scheme,
                    "host" to context.req.host,
                ))
            }
            "requestHeaders" -> {
                return context.res.json(context.req.headers as MutableMap<String, Any>)
            }
            "requestBodyText" -> {
                return context.res.text(context.req.body as String)
            }
            "requestBodyJson" -> {
                val key1: String
                val key2: String

                if(context.req.body is String) {
                    key1 = "Missing key"
                    key2 = "Missing key"
                } else {
                    val body: MutableMap<String, Any> = context.req.body as MutableMap<String, Any>

                    key1 = body.getOrDefault("key1", "Missing key").toString();
                    key2 = body.getOrDefault("key2", "Missing key").toString();
                }

                return context.res.json(mutableMapOf(
                    "key1" to key1,
                    "key2" to key2,
                    "raw" to context.req.bodyRaw
                ))
            }
            "envVars" -> {
                return context.res.json(mutableMapOf(
                    "var" to System.getenv().getOrDefault("CUSTOM_ENV_VAR", null),
                    "emptyVar" to System.getenv().getOrDefault("NOT_DEFINED_VAR", null)
                ));
            }
            "logs" -> {
                System.out.println("Native log");
                context.log("Debug log");
                context.error("Error log");
                
                context.log("Log+With+Plus+Symbol");

                context.log(42);
                context.log(4.2);
                context.log(true);

                context.log(mutableListOf(
                    "arrayValue"
                ));

                context.log(mutableMapOf(
                    "objectKey" to "objectValue"
                ));

                return context.res.text("");
            }
            "library" -> {
                val gson = Gson()

                val url = URL("https://jsonplaceholder.typicode.com/todos/" + context.req.bodyRaw)
                val con = (url.openConnection() as HttpURLConnection).apply {
                    requestMethod = "GET"
                    responseCode
                }

                val todoString = buildString {
                    BufferedReader(InputStreamReader(con.inputStream)).useLines { lines ->
                        lines.forEach { append(it) }
                    }
                }

                con.disconnect()

                val todo = gson.fromJson<Map<String, Any>>(
                    todoString,
                    MutableMap::class.java
                )

                return context.res.json(mutableMapOf(
                    "todo" to todo
                ))
            }
            "timeout" -> {
                context.log("Timeout start.")

                coroutineScope {
                    delay(3000)
                }

                context.log("Timeout end.")
                return context.res.text("Successful response.")
            }
            else -> {
                throw Exception("Unknown action")
            }
        }
    }
}
