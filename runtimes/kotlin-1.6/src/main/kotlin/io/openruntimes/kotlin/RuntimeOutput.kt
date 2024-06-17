package io.openruntimes.kotlin

data class RuntimeOutput(
    val body: ByteArray,
    val statusCode: Int,
    val headers: Map<String, String>
) {
    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as RuntimeOutput

        if (!body.contentEquals(other.body)) return false
        if (statusCode != other.statusCode) return false
        if (headers != other.headers) return false

        return true
    }

    override fun hashCode(): Int {
        var result = body.contentHashCode()
        result = 31 * result + statusCode
        result = 31 * result + headers.hashCode()
        return result
    }
}