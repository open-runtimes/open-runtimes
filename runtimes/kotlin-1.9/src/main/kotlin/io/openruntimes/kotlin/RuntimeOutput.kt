package io.openruntimes.kotlin

data class RuntimeOutput(
    val body: Any,
    val statusCode: Int,
    val headers: Map<String, String>
)