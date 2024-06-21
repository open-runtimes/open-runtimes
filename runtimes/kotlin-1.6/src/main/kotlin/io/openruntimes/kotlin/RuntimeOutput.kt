package io.openruntimes.kotlin

data class RuntimeOutput(
    val body: ByteArray,
    val statusCode: Int,
    val headers: Map<String, String>,
    val chunked: Boolean
) {
}
