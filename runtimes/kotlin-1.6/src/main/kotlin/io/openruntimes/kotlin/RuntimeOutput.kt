package io.openruntimes.kotlin

data class RuntimeOutput(
    val body: String,
    val statusCode: Int,
    val headers: Map<String, String>
)