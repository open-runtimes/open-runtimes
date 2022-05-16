import com.google.gson.Gson
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.HttpURLConnection
import java.net.URL

private val gson = Gson()

@Throws(Exception::class)
fun main(req: RuntimeRequest, res: RuntimeResponse): RuntimeResponse {

    val payload = gson.fromJson<Map<String, Any>>(
        req.payload.ifEmpty { "{}" },
        MutableMap::class.java
    )

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

    return res.json(mapOf(
        "message" to "Hello Open Runtimes 👋",
        "todo" to todo
    ))
}