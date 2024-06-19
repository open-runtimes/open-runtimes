package io.openruntimes.kotlin

data class RuntimeOutput(
    val body: UByteArray,
    val statusCode: Int,
    val headers: Map<String, String>
) {
}
