package io.openruntimes.kotlin

object OprConfig {
    val secret: String = System.getenv("OPEN_RUNTIMES_SECRET") ?: ""
    val headers: Map<String, String> =
        try {
            gsonInternal
                .fromJson(System.getenv("OPEN_RUNTIMES_HEADERS") ?: "{}", MutableMap::class.java)
                .entries
                .associate { "${it.key}".lowercase() to "${it.value}" }
        } catch (e: Exception) {
            emptyMap()
        }
    val entrypoint: String = System.getenv("OPEN_RUNTIMES_ENTRYPOINT") ?: ""
    val env: String = System.getenv("OPEN_RUNTIMES_ENV") ?: ""
}
