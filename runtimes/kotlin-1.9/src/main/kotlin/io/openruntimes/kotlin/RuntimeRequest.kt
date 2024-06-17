package io.openruntimes.kotlin

data class RuntimeRequest(
    val method: String,
    val scheme: String,
    val host: String,
    val port: Int,
    val path: String,
    val query: MutableMap<String, String>,
    val queryString: String,
    val headers: MutableMap<String, String>,
    val bodyBinary: UByteArray,
    val url: String,
) {
    val bodyText: String
        get() = String(bodyBinary)
    @Suppress("UNCHECKED_CAST")
    val bodyJson: MutableMap<String, Any>
        get() = gson.fromJson(bodyText, MutableMap::class.java) as MutableMap<String, Any>
    val bodyRaw: String
        get() = bodyText
    val body: Any
        get() {
            val contentType = headers.getOrDefault("content-type", "text/plain").lowercase()
            val binaryTypes = arrayOf("application/", "audio/", "font/", "image/", "video/")

            return when {
                contentType.startsWith("application/json") -> bodyJson
                binaryTypes.any { contentType.startsWith(it) } -> bodyBinary
                else -> return bodyText
            }
        }
}
