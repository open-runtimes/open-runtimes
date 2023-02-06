package io.openruntimes.kotlin

import com.google.gson.Gson
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.HttpURLConnection
import java.net.URL

public class Index {
    @Throws(Exception::class)
    fun main(context: RuntimeContext): RuntimeOutput {
        val gson = Gson()

        val payload = context.req.body as MutableMap<String, Any>

        val id = payload["id"] ?: "1"

        val url = URL("https://jsonplaceholder.typicode.com/todos/$id")
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
            "message" to "Hello Open Runtimes 👋",
            "todo" to todo
        ))
    }
}
