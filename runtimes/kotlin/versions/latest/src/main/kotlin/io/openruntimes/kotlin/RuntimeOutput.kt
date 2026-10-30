package io.openruntimes.kotlin

import kotlinx.coroutines.flow.Flow

data class RuntimeOutput(
    val body: ByteArray,
    val statusCode: Int,
    val headers: Map<String, String>,
    val stream: Flow<Any>? = null,
)
