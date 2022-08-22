import com.google.gson.Gson
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.HttpURLConnection
import java.net.URL

val gson = Gson()

@Throws(Exception::class)
fun main(req: RuntimeRequest, res: RuntimeResponse): RuntimeResponse {

    val payload = gson.fromJson<Map<String, Any>>(
        if (req.payload.isEmpty()) "{}" else req.payload,
        MutableMap::class.java
    )

    val header = req.headers["x-test-header"] ?: ""
    val env = req.env["test-env"] ?: ""
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

    // Print to user function stdout
    println("log1")
    println("{hello: world}")
    println("[hello, world]")

    return res.json(mapOf(
        "isTest" to true,
        "message" to "Hello Open Runtimes 👋",
        "header" to header,
        "env" to env,
        "null-env" to req.env["test-env"],
        "todo" to todo
    ))
}