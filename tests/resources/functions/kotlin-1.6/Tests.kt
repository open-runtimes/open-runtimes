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
    val varData = req.variables["test-variable"] ?: ""
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

    System.out.println("String1");
    System.out.println(42);
    System.out.println(4.2);
    System.out.println(true);

    System.out.println("String2");
    System.out.println("String3");
    System.out.println("String4");
    System.out.println("String5");

    return res.json(mapOf(
        "isTest" to true,
        "message" to "Hello Open Runtimes 👋",
        "header" to header,
        "variable" to varData,
        "todo" to todo
    ))
}